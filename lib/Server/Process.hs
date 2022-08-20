{- |
All code about applying Responses to a noun, and then parsing the produced
keys and Requests from the return value.

We split this code out because it is the same between the SimpleMachine and
any more complicated production Machine we write.
-}
module Server.Process where

import PlunderPrelude

import Control.Monad.State (StateT, evalStateT, runStateT)
import Numeric.Natural     (Natural)
import Plun                (Nat, Fan(NAT), (%%))
import System.Entropy      (getEntropy)

import Server.Convert
import Server.Time
import Server.Types.Logging

import qualified Data.Set    as S
import qualified Data.Vector as V
import qualified Plun        as P

thirtySecondsInUs :: Int = 30 * 10 ^ 6

-- Types needed to communicate with an individual process.

-- -----------------------------------------------------------------------

-- An open request is a request on a given thread.
data RequestHandle = RequestHandle { rhPid :: ProcessId, rhReqId :: RequestId }
  deriving (Eq, Ord, Show)

--
recvHandle :: ProcessId -> RequestHandle
recvHandle pid = RequestHandle pid (RequestId 0)

-- The types of Request we can parse
data Request
  -- Local Operations
  = ReqCall Nat Fan        -- [0 i v] -> IO Fan
  | ReqSend ProcessId Fan  -- [1 p v] -> IO ()
  | ReqFork Fan            -- [2 v]   -> IO Pid

  -- We can't parse this request, but we must still track it. The Fan is the
  -- raw request val.
  | UNKNOWN Fan
  deriving (Show)

-- A response is the input to the process.
data Response
  = RespRecv {
      respRecvSendReq :: RequestHandle,
      respRecvVal     :: Fan
      }
  | RespFork {
      respForkPid :: ProcessId,
      respForkVal :: Fan
      }
  | RunValue {
      rvVal :: Fan
      }
  deriving (Show)

responseOK :: Response
responseOK = RunValue $ NAT 0

responseToVal :: Response -> Fan
responseToVal RespRecv{..}    = P.mkRow [toNoun $ rhPid respRecvSendReq,
                                         respRecvVal]
responseToVal RespFork{..}    = toNoun respForkPid
responseToVal RunValue{rvVal} = rvVal

-- -----------------------------------------------------------------------

-- | The state of a Process.
data Process = PROCESS {
  processProcessId       :: ProcessId,
  processNoun            :: Fan,
  processRequestsByIndex :: IntMap (Fan, Request)
  }
  deriving (Show)

-- | Describes the effect running the response had. Different execeffects can
-- result in different Receipts, and may require different behaviours when
-- updating Machine state.
data ExecEffect
  -- | We didn't receive a response (ie, in the case of a new forked process).
  -- The attached changeset is just adds. This will write `ReceiptInit` to the
  -- event log.
  = EEInit { initPid :: ProcessId, initVal :: P.Fan }

  -- | When the `Response` is a `Fork`, we also have a new forked thread that
  -- has to be added to Machine state. A `ReceiptFork` may be
  | EEForked { forkedRequestId :: RequestId,
               forkedProcess   :: Process,
               forkedChangeset :: ExecChangeset }

  -- | When the `Response` is to the implicit 0th Recv, we can record a
  -- `ReceiptRecv` in the corresponding event log instead of a raw
  -- `ReceiptVal`.
  | EERecv { recvRequestId :: RequestId,
             recvVal       :: Fan,
             causingSend   :: RequestHandle }

  -- | The response applied a reqId/Val to the process, and will write a
  -- `ReceiptVal` to the event log.
  | EEDefault { execId  :: RequestId,
                execVal :: Fan }

  -- | Running this request crashed, either because of an explicit crash or
  -- because the event hit a timeout or a memory exhaustion situation.
  | EECrashed
  deriving (Show)

-- | All the changes that occurred by running a `Response`.
data ExecChangeset = ExecChangeset {
  execChangesetProcessId       :: ProcessId,

  execChangesetAlive           :: Bool,
  execChangesetAddedRequests   :: [(RequestHandle, Request)],
  execChangesetRemovedRequests :: [(RequestHandle, Request)],

  -- Response specific effect: A positive
  execChangesetExecEffect      :: ExecEffect
  }
  deriving (Show)

-- All the changes that occurred in response to reloading from a snapshot.
data ReloadChangeset = ReloadChangeset {
  reloadChangesetProcessId     :: ProcessId,
  reloadChangesetAddedRequests :: [(RequestHandle, Request)]
  }
  deriving (Show)

-- -----------------------------------------------------------------------

makeFieldLabels ''Process
makeFieldLabels ''ExecChangeset
makeFieldLabels ''ReloadChangeset

getCell :: Fan -> Maybe (Fan, Fan)
getCell v@P.KLO{} = Just (P.boom v)
getCell _         = Nothing

-- -----------------------------------------------------------------------

valToRequest :: Fan -> Request
valToRequest v@(P.ROW xs) =
  fromMaybe (UNKNOWN v) $ case toList xs of
    [NAT 0, dst, val] -> ReqCall <$> fromNoun dst <*> pure val
    [NAT 1, dst, val] -> ReqSend <$> fromNoun dst <*> pure val
    [NAT 2, exe]      -> pure $ ReqFork exe
    _                 -> Nothing
valToRequest v = UNKNOWN v

newProcess :: ProcessId -> Fan -> Process
newProcess aid v = PROCESS {
  processProcessId = aid,
  processNoun = v,
  processRequestsByIndex = mempty
  }

reloadProcessSnapshot :: (ProcessId, Fan)
                      -> IO (ReloadChangeset, Process)
reloadProcessSnapshot (pid, val) = do
  (r, s) <- runStateT parseReloadChangeset (newProcess pid val)
  pure (r, s)

buildInitialProcess :: ProcessId -> Fan -> IO (ExecChangeset, Process)
buildInitialProcess pid val = do
  let init = force (val %% toNoun pid)
  runStateT (parseExecChangeset (EEInit pid val))
            (newProcess pid init)

execResponse :: RequestHandle -> Response
             -> StateT Process IO (Maybe ExecChangeset)
execResponse reqH response = do
  validateHandle $ \reqId -> do
    let respVal = responseToVal response

    let exe s = force (s %% (toNoun reqId) %% respVal)

    noun <- use #noun
    result <- liftIO $ timeout thirtySecondsInUs (pure (exe noun))

    case result of
      Nothing -> Just <$> makeTimeout
      Just result -> do
        assign' #noun result

        execEffect <- case response of
          (RespRecv sendReqh val) -> pure $ EERecv reqId val sendReqh
          (RespFork aid val)      -> makeEEForked reqId aid val
          _                       -> pure $ EEDefault reqId respVal

        Just <$> parseExecChangeset execEffect
  where
    validateHandle fun = do
      let (RequestHandle _ reqId@(RequestId id)) = reqH
      case id of
        0 -> fun reqId
        _ -> do
          byIndex <- use #requestsByIndex
          case lookup id byIndex of
            Nothing             -> pure Nothing
            Just (storedVal, r) -> fun reqId

    makeEEForked reqId pid val = do
      let init = force (val %% toNoun pid)
      (ec, process) <- liftIO $ runStateT
        (parseExecChangeset (EEInit pid val))
        (newProcess pid init)
      pure $ EEForked reqId process ec

    makeTimeout = do
      execChangesetProcessId <- use #processId
      let execChangesetAlive = True
          execChangesetAddedRequests = mempty
          execChangesetRemovedRequests = mempty
          execChangesetExecEffect = EECrashed
      pure ExecChangeset{..}

type ReqChange = Either (RequestHandle, Request) (RequestHandle, Request)

-- We must parse the
parseExecChangeset :: ExecEffect
                   -> StateT Process IO ExecChangeset
parseExecChangeset execChangesetExecEffect = do
  execChangesetProcessId <- use #processId
  (execChangesetAddedRequests, execChangesetRemovedRequests) <-
    parseRequests

  reqNoun <- getCurrentReqNoun
  let execChangesetAlive = isJust reqNoun
  pure ExecChangeset{..}

parseReloadChangeset :: StateT Process IO ReloadChangeset
parseReloadChangeset = do
  reloadChangesetProcessId <- use #processId

  -- We associate all requests with the last request in the changelog.
  (reloadChangesetAddedRequests, _) <- parseRequests

  pure ReloadChangeset{..}

parseRequests :: StateT Process IO ([(RequestHandle, Request)],
                                  [(RequestHandle, Request)])
parseRequests = do
  expected <- use #requestsByIndex
  requests <- fromMaybe mempty <$> getCurrentReqNoun

  changed <- for (mapToList expected) $ \(i,(v,_)) ->
      case (requests V.!? (i - 1)) of
          Nothing       -> cancelReq i
          Just w | v==w -> pure []
          Just w        -> (++) <$> cancelReq i <*> createReq i w

  new <- for (zip [1..] (toList requests)) $ \(i,v) ->
      case member i expected of
        False -> createReq i v
        True  -> pure []

  let allReqs = partitionEithers $ concat $ changed ++ new
  pure allReqs
  where
    createReq :: Int -> Fan
              -> StateT Process IO [ReqChange]
    createReq i v = do
      let req = valToRequest v
      aid <- use #processId
      let id = RequestId i
      let handle = RequestHandle aid id
      modifying #requestsByIndex (insertMap i (v, req))
      pure $ [Left (handle, req)]

    cancelReq :: Int -> StateT Process IO [ReqChange]
    cancelReq key = do
      Just (v, req) <- use ( #requestsByIndex % at key )
      aid <- use #processId
      let handle = RequestHandle aid (RequestId key)
      modifying #requestsByIndex (deleteMap key)
      pure $ [Right (handle, req)]

getCurrentReqNoun :: StateT Process IO (Maybe (Vector Fan))
getCurrentReqNoun = do
  s <- use (to processNoun)
  pure $ do
    (_, req) <- getCell s
    P.getRow req
