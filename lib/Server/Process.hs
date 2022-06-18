module Server.Process where

-- All code about applying Responses to a noun, and then parsing the produced
-- keys and Requests from the return value.
--
-- We split this code out because it is the same between the SimpleMachine and
-- any more complicated production Machine we write.

import PlunderPrelude

import Control.Monad.State (StateT, evalStateT, runStateT)
import Crypto.Sign.Ed25519 (PublicKey(..), SecretKey(..), createKeypair,
                            toPublicKey)
import Numeric.Natural     (Natural)
import Plun                (Nat, Val, pattern AT, (%%))
import Plun.Print          (encodeBtc)
import System.Entropy      (getEntropy)

import Server.Convert
import Server.Time
import Server.Types.Logging

import qualified Data.Set    as S
import qualified Data.Vector as V
import qualified Plun        as P

thirtySecondsInUs :: Int = 30 * 10 ^ 6

-- Types needed to communicate with an individual process.

newtype Key = KEY { unkey :: SecretKey }
  deriving newtype (Eq, Ord)

instance Show Key where
  show _ = "Key {<SECRET>}"

newtype Pub = PUB { unpub :: PublicKey }
  deriving newtype (Eq, Ord)

instance Show Pub where
  show (PUB (PublicKey bs)) = "Pub {" ++ (unpack $ encodeBtc bs) ++ "} "

newtype LocalAddress = LOCAL { unLocal :: Nat }
  deriving newtype (Show, Eq, Ord)

--
data Claim
  = LocalClaim LocalAddress  -- 0
  | Ed25519Blake3Claim Key   -- 1
  deriving (Show, Eq, Ord)

-- -----------------------------------------------------------------------

data LogType = Logged | Unlogged
  deriving (Eq, Ord, Show)

-- ProcessIdxs are written to event logs. But we have to be able to address
-- processes which aren't written to event logs, too.
data ProcessId = ProcessId LogType ProcessIdx
  deriving (Eq, Ord, Show)

processIdToProcessIdx :: ProcessId -> Either ProcessIdx ProcessIdx
processIdToProcessIdx (ProcessId Logged l)   = Left l
processIdToProcessIdx (ProcessId Unlogged r) = Right r

-- -----------------------------------------------------------------------

-- An open request is a request on a given thread. Two OpenReq are equal if
-- they point to the same position on the same thread.
data RequestHandle = RequestHandle ProcessId RequestIdx
  deriving (Eq, Ord, Show)

data LocalSendRep = LocalSendRep { srSrc :: LocalAddress,
                                   srDst :: LocalAddress,
                                   srVal :: Val }
  deriving (Show)

data LocalRecvRep = LocalRecvRep { rrDst  :: LocalAddress,
                                   rrSrcs :: Set LocalAddress }
  deriving (Show)

-- The types of Request we can parse
data Request
  = LocalNext                  -- 1               -> IO Nat
  | ForkLogged Val             -- {2 Exe}         -> IO {}
  | ForkUnlogged Val           -- {3 Exe}         -> IO {}
  | LocalBury LocalAddress     -- {4 Exe}         -> IO Why
  | LocalKill LocalAddress Val -- {5 Nat Val}     -> IO {}
  | LocalSend LocalSendRep     -- {6 Nat Nat Val} -> IO {}
  | LocalRecv LocalRecvRep     -- {7 Nat Nat*}    -> IO {Nat Val}
  | Wait Natural               -- {8 Wen}         -> IO Wen
  | When                       -- 9               -> IO Wen
  | Rand                       -- 10              -> IO Bar
  -- We can't parse this request, but we must still track it. The Val is the
  -- raw request val.
  | UNKNOWN Val
  deriving (Show)

data BuryWhy
  = WhyKilled Val
  | WhyShutdown
  | WhyMissing
  | WhyReleased
  | WhyMemory
  | WhyTimeout
  deriving (Show)


data ForkOp
  = FOLogged ProcessId
  | FOUnlogged ProcessId
  | FOReplayUnlogged
  deriving (Show)

-- A response is the input to the process.
data Response
  = Fork {
      fOp  :: ForkOp,
      fVal :: Val
      }
  | RunValue {
      rlVal :: Val
      }
  | RecvLocal {
      rlSendReq :: RequestHandle,
      rlVal     :: Val,
      rlSrc     :: LocalAddress,
      rlDst     :: LocalAddress
      }
  | RunKill {
      rlReason           :: Val,
      rlLoggedTidsKilled :: [ProcessIdx],
      rlKilledChangesets :: [KilledChangeset]
      }
  deriving (Show)

responseOK :: Response
responseOK = RunValue $ AT 0

responseToVal :: Response -> IO Val
responseToVal Fork{}                = pure $ AT 0
responseToVal (RunValue val)        = pure val
responseToVal (RecvLocal _ val _ _) = pure val
responseToVal RunKill{}             = pure $ AT 0

-- -----------------------------------------------------------------------

data Process = PROCESS {
  processProcessId         :: ProcessId,
  processNoun              :: Val,
  processRequestsByIndex   :: IntMap (Val, Request),
  processExpectedClaimNoun :: Val
  }
  deriving (Show)

-- Describes the effect running the response had. Different execeffects can
-- result in different Receipts, and may require different behaviours when
-- updating Machine state.
data ExecEffect
  -- We didn't apply a value (ie, in the case of a new forked process).  The
  -- attached changeset is just adds.
  = EEInit { initAid :: ProcessId, initVal :: P.Val }

  -- The response applied a reqId/Val to the process.
  | EEDefault { execIdx :: RequestIdx,
                execVal :: Val }
  -- When the Response is a fork, we also have a new forked thread that has to
  -- be processed.
  | EEForked { forkedRequestIdx :: RequestIdx,
               forkedProcess    :: Process,
               forkedChangeset  :: ExecChangeset,
               forkedType       :: LogType
               }
  | EELocalRecv { recvRequestIdx :: RequestIdx,
                  recvVal        :: Val,
                  causingSend    :: RequestHandle,
                  recvFrom       :: LocalAddress,
                  recvTo         :: LocalAddress
             -- TODO: This needs to deal with remote sends later. Do we have to
             -- recv exec effects?
             }
  -- The response was a kill, and on applying the changeset, we must kill all
  -- matching processes.
  | EEKilled { killRequestIdx       :: RequestIdx,
               killReason           :: Val,
               killLoggedTidsKilled :: [ProcessIdx],
               killKilledChangesets :: [KilledChangeset]
             }
  -- Running this request crashed, either because of an explicit crash or
  -- because the event hit a timeout or a memory exhaustion situation.
  | EECrashed
  deriving (Show)

-- An process can go from nothing to dead
data LivelinessChange
  = StillAlive
  | Shutdown
  | MemoryExhaustion
  | Timeout
  deriving (Eq, Show)

-- All the changes that occurred after a Response.
--
data ExecChangeset = ExecChangeset {
  execChangesetProcessId       :: ProcessId,

  execChangesetAlive           :: LivelinessChange,
  execChangesetAddedClaims     :: [Claim],
  execChangesetRemovedClaims   :: [Claim],
  execChangesetAddedRequests   :: [(RequestHandle, Request)],
  execChangesetRemovedRequests :: [(RequestHandle, Request)],

  -- Response specific effect: A positive
  execChangesetExecEffect      :: ExecEffect

  -- library change information goes here when that gets added.
  }
  deriving (Show)

-- All the changes that occurred in response to reloading from a snapshot.
data ReloadChangeset = ReloadChangeset {
  reloadChangesetProcessId     :: ProcessId,

  reloadChangesetAddedClaims   :: [Claim],
  reloadChangesetAddedRequests :: [(RequestHandle, Request)]
  }
  deriving (Show)

-- All the changes that occurred when an unlogged process is killed by a
data KilledChangeset = KilledChangeset {
  killedChangesetProcessId       :: ProcessId,
  killedChangesetRemovedClaims   :: [Claim],
  killedChangesetRemovedRequests :: [(RequestHandle, Request)]
  }
  deriving (Show)

-- -----------------------------------------------------------------------

makeFieldLabels ''Process
makeFieldLabels ''ExecChangeset
makeFieldLabels ''ReloadChangeset
makeFieldLabels ''KilledChangeset

-- -----------------------------------------------------------------------

instance ToNoun Pub where
  toNoun (PUB (PublicKey b)) = toNoun b

instance FromNoun Pub where
  fromNoun n = do
    b <- fromNoun n
    guard (length b == 32)
    pure (PUB $ PublicKey b)

instance ToNoun Key where
  toNoun (KEY (SecretKey b)) = toNoun b

instance FromNoun Key where
  fromNoun n = do
    b <- fromNoun n
    guard (length b == 64)
    pure (KEY $ SecretKey b)

instance ToNoun LocalAddress where
  toNoun (LOCAL a) = toNoun a

instance FromNoun LocalAddress where
  fromNoun n = LOCAL <$> fromNoun n

instance ToNoun Claim where
  toNoun (LocalClaim la)        = P.mkRow [toNoun @Nat 0, toNoun la]
  toNoun (Ed25519Blake3Claim k) = P.mkRow [toNoun @Nat 1, toNoun k]

instance FromNoun Claim where
  fromNoun n = do
    r <- getRawRow n
    claimType <- r V.!? 0 >>= fromNoun @Nat
    case claimType of
      0 -> LocalClaim <$> (r V.!? 1 >>= fromNoun)
      1 -> Ed25519Blake3Claim <$> (r V.!? 1 >>= fromNoun)
      _ -> Nothing

instance ToNoun BuryWhy where
  toNoun (WhyKilled val) = P.mkRow [AT 0, val]
  toNoun WhyShutdown     = AT 1
  toNoun WhyMissing      = AT 2
  toNoun WhyReleased     = AT 3
  toNoun WhyMemory       = AT 4
  toNoun WhyTimeout      = AT 5

getCell :: Val -> Maybe (Val, Val)
getCell (P.VAL _ (P.APP h t)) = Just (P.nodVal h, t)
getCell _                     = Nothing

-- -----------------------------------------------------------------------

valToRequest :: Val -> Request
valToRequest n = fromMaybe (UNKNOWN n) $ do
  case n of
    --
    (P.VAL _ (P.NAT n)) -> case n of
      -- 1               -> IO {}        [ local_next ]
      1  -> pure LocalNext
      -- 9               -> IO Wen       [ when ]
      9  -> pure When
      -- 10              -> IO Bar       [ rand ]
      10 -> pure Rand
      _  -> Nothing

    (P.VAL _ (P.DAT (P.ROW xs))) -> case toList xs of
      -- {2 Exe}         -> IO {}         [ local_fork_log ]
      [AT 2, exe] -> pure $ ForkLogged exe

      -- {3 Exe}         -> IO {}         [ local_fork_unlog ]
      [AT 3, exe] -> pure $ ForkUnlogged exe

      -- {4 Nat}         -> IO Why        [ local_bury ]
      [AT 4, srcN] -> LocalBury <$> fromNoun srcN

      -- {5 Nat Val}     -> IO {}         [ local_kill ]
      [AT 5, dstN, val] -> LocalKill <$> fromNoun dstN <*> pure val

      -- {6 Nat Nat Val} -> IO {}         [ local_send ]
      [AT 6, srcN, dstN, bodyN] -> do
        src <- fromNoun srcN
        dst <- fromNoun dstN
        pure $ LocalSend $ LocalSendRep src dst bodyN

      -- {7 Nat Nat*}    -> IO {Nat Val}  [ local_recv ]
      [AT 7, srcN, dstNs] -> do
        src <- fromNoun srcN
        dstVec <- P.getRow dstNs
        recv <- traverse fromNoun dstVec
        pure $ LocalRecv $ LocalRecvRep src (setFromList $ V.toList recv)

      -- {8 Wen}         -> IO Wen        [ wait ]
      [AT 8, timeN] -> Wait <$> fromNoun timeN

      _ -> Nothing

newProcess :: ProcessId -> Val -> Process
newProcess aid v = PROCESS {
  processProcessId = aid,
  processNoun = v,
  processRequestsByIndex = mempty,
  processExpectedClaimNoun = P.mkRow []
  }

readProcess :: Val -> Maybe (Natural, Val)
readProcess n = do
  r <- getRawRow n
  lastResp <- r V.!? 0 >>= fromNoun
  noun <- r V.!? 1
  pure (lastResp, noun)

reloadProcessSnapshot :: (ProcessIdx, Val)
                      -> IO (ReloadChangeset, Process)
reloadProcessSnapshot (pidx, val) = do
  let pid = ProcessId Logged pidx
  (r, s) <- runStateT parseReloadChangeset (newProcess pid val)
  pure (r, s)

killProcess :: Process -> IO KilledChangeset
killProcess process = evalStateT exec process
  where
    exec :: StateT Process IO KilledChangeset
    exec = do
      assign' #noun (AT 0)
      killedChangesetProcessId <- use #processId
      (_, killedChangesetRemovedClaims) <- parseClaims
      (_, killedChangesetRemovedRequests) <- parseRequests
      pure KilledChangeset{..}


buildInitialProcess :: ProcessIdx -> Val -> IO (ExecChangeset, Process)
buildInitialProcess pidx val = do
  let pid = ProcessId Logged pidx
  runStateT (parseExecChangeset (EEInit pid val))
            (newProcess pid val)

execResponse :: RequestHandle -> Response
             -> StateT Process IO (Maybe ExecChangeset)
execResponse reqH response = do
  validateHandle $ \reqIdx -> do
    respVal <- liftIO $ responseToVal response

    let exe s = force (s %% (toNoun reqIdx) %% respVal)

    noun <- use #noun
    result <- liftIO $ timeout thirtySecondsInUs (pure (exe noun))

    case result of
      Nothing -> Just <$> makeTimeout
      Just result -> do
        assign' #noun result

        execEffect <- case response of
          (Fork op val) -> case op of
            FOLogged aid     -> makeEEForked reqIdx aid val Logged
            FOUnlogged aid   -> makeEEForked reqIdx aid val Unlogged
            FOReplayUnlogged -> pure $ EEDefault reqIdx respVal
          (RecvLocal sendAt val src dst) ->
            pure $ EELocalRecv reqIdx respVal sendAt src dst
          (RunKill reason pids changes) ->
            pure $ EEKilled reqIdx reason pids changes
          _ -> pure $ EEDefault reqIdx respVal

        Just <$> parseExecChangeset execEffect
  where
    validateHandle fun = do
      let (RequestHandle _ reqIdx@(RequestIdx idx)) = reqH
      byIndex <- use #requestsByIndex
      case lookup idx byIndex of
        Nothing             -> pure Nothing
        Just (storedVal, r) -> fun reqIdx

    makeEEForked reqIdx aid val t = do
      (ec, process) <- liftIO $ runStateT
        (parseExecChangeset (EEInit aid val))
        (newProcess aid val)
      pure $ EEForked reqIdx process ec t

    makeTimeout = do
      -- TODO: This needs to deal with logged vs unlogged. In the case of
      -- unlogged processes, we need to clean up all requests and claims.
      execChangesetProcessId <- use #processId
      let execChangesetAlive = Timeout
          execChangesetAddedClaims = mempty
          execChangesetRemovedClaims = mempty
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
  (execChangesetAddedClaims, execChangesetRemovedClaims) <- parseClaims
  (execChangesetAddedRequests, execChangesetRemovedRequests) <-
    parseRequests

  reqs <- use #requestsByIndex
  let execChangesetAlive = case null reqs of
        True  -> Shutdown
        False -> StillAlive
        -- TODO: MemoryExhaustion are unimplemented in the haskell
  pure ExecChangeset{..}

parseReloadChangeset :: StateT Process IO ReloadChangeset
parseReloadChangeset = do
  reloadChangesetProcessId <- use #processId
  (reloadChangesetAddedClaims, _) <- parseClaims

  -- We associate all requests with the last request in the changelog.
  (reloadChangesetAddedRequests, _) <- parseRequests

  pure ReloadChangeset{..}

parseRequests :: StateT Process IO ([(RequestHandle, Request)],
                                  [(RequestHandle, Request)])
parseRequests = do
  -- Maybe what's wrong here is that getCurrentReqNoun doesn't pull the
  -- right thing off processNoun.
  expected <- use #requestsByIndex
  requests <- use (to (fromMaybe mempty . getCurrentReqNoun . processNoun) )

  changed <- for (mapToList expected) $ \(i,(v,_)) ->
      case (requests V.!? i) of
          Nothing       -> cancelReq i
          Just w | v==w -> pure []
          Just w        -> (++) <$> cancelReq i <*> createReq i w

  new <- for (zip [0..] (toList requests)) $ \(i,v) ->
      case member i expected of
        False -> createReq i v
        True  -> pure []

  let allReqs = partitionEithers $ concat $ changed ++ new
  pure allReqs
  where
    createReq :: Int -> Val
              -> StateT Process IO [ReqChange]
    createReq i v = do
      let req = valToRequest v
      aid <- use #processId
      let idx = RequestIdx i
      let handle = RequestHandle aid idx
      modifying #requestsByIndex (insertMap i (v, req))
      pure $ [Left (handle, req)]

    cancelReq :: Int -> StateT Process IO [ReqChange]
    cancelReq key = do
      Just (v, req) <- use ( #requestsByIndex % at key )
      aid <- use #processId
      let handle = RequestHandle aid (RequestIdx key)
      modifying #requestsByIndex (deleteMap key)
      pure $ [Right (handle, req)]

    -- %req
    getCurrentReqNoun s = do
      (_, req) <- getCell s
      P.getRow req

parseClaims :: StateT Process IO ([Claim], [Claim])
parseClaims = do
  n <- use #noun
  current <- getCurrentClaimNoun <$> use #noun
  expected <- use #expectedClaimNoun
  if current == expected then pure ([], [])
  else do
    let currentS = rowToClaimSet current
        expectedS = rowToClaimSet expected

        addedClaims = S.toList $ S.difference currentS expectedS
        removedClaims = S.toList $ S.difference expectedS currentS

    assign #expectedClaimNoun current
    pure (addedClaims, removedClaims)
  where
    getCurrentClaimNoun s = fromMaybe (P.mkRow []) $ do
      (inner, req) <- getCell s
      (_, keys) <- getCell inner
      pure keys

    rowToClaimSet :: Val -> Set Claim
    rowToClaimSet (P.VAL _ (P.DAT (P.ROW v))) =
        S.fromList $ catMaybes $ fmap fromNoun $ V.toList v
    rowToClaimSet _                            = mempty


isLogged :: StateT Process IO Bool
isLogged = use #processId >>= \case
  ProcessId Logged _   -> pure True
  ProcessId Unlogged _ -> pure False
