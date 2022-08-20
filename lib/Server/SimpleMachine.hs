{- |

= Overview

A Plunder Machine is made up of multiple processes. A `Machine` has to keep
track of communication between all the `Process`es that make up the Machine. It
communicates with the `LmdbStore` to write `LogBatch`es, and with `Router` to
communicate with other Machines.

This is the simplest Machine implementation. It is meant to be a simplification
for teaching and not an optimized implementation for production use, though it
should always produce the same results. This implementation blocks on
everything. See Caveats.

== Lifecycle

A Machine's lifecycle is that it is booted by just writing a log entry with one
`ReceiptInit` event. Machine creation doesn't even happen in this code.

SimpleMachines are thus always constructed by replaying an event log with
`replayMachine`. This is the entry point to this entire module, which spawns
its own async to run all the `Process` in this Machine. From the point of view
of the caller, it now has a `MachineHandle` to send message command or to wait
on an Async. replayMachine will execute all `LogBatch`es in the machine's event
log to recreate its consistent state.

From that point on, everything happens in `mainloop`. See the internal
documentation for details.

Runtime is then spent in the `mainloop`. One next work item is retrieved with
`getNextWork`. That's handled and then it recurses (except in the one case
where work is a shutdown command).

`Machine` is a large bundle of all the state. It is mostly indexes of


= Caveats

In the summer of 2021, we put together a pure actor model based system, based
around an ordering process whose main job was to produce one total event log
ordering from the results of multiple asyncs so you had execution
parallelization while also having event log replayability to the exact same
state. This implementation had a bunch of optimizations for parallelism and
throughput.

This simplified version does not have any of those optimizations. Eventually,
we intend to write a production version which has all the parallelism and
efficiency optimizations we proved out in a previous pure actor model variant
of the system. But until that time, this simplified version:

- Does absolutely no parallelism between processes. This is entirely
  single-threaded, running all processes one at a time. The production
  version has every process as an async, but that complicates the code quite
  a bit.

- Pauses the world to take a snapshot. The production version has a ton of
  complexity around keeping asynchronous processes processing events while a
  coherent snapshot of the system is being taken. Unlike the production
  version, this just stops the world.

- We also stop the world entirely when writing the event log to disk. This
  simplifies response handling. The production version immediately releases
  response that can't be observed outside of the Machine.

- We run only one event per log batch. This simplifies response queuing
  logic, which otherwise has to keep track of what responses are queued for
  what requests. The production version runs as many events as it can to
  stuff into a log batch while waiting on the disk to commit the previous
  log batch.

- Implements no real scheduling. In an attempt at fairness, this uses a
  simplified random ordering of outstanding possible responses. The
  production version prioritizes some responses over others for smoothness
  and queuing reasons.

- Implements no queue bounding. Unbounded queues on networks are a stability
  issue in producer/consumer systems and this simple implementation does
  none of the necessary work due to the complexity.

-}
module Server.SimpleMachine
    ( replayMachine
    )
where

import PlunderPrelude

import Control.Concurrent.STM.Delay
import Control.Monad.State          (MonadState, StateT, evalStateT, execState,
                                     get, gets, modify', put, runStateT)
import Control.Monad.STM            (retry)
import Data.IntMap                  (IntMap)
import Numeric.Natural              (Natural)
import Optics
import Optics.At
import Optics.State.Operators
import Optics.TH
import Optics.Zoom
import Plun                         (Nat, Fan(NAT))
import Plun.Print                   (encodeBtc)
import System.Entropy               (getEntropy)
import System.Random                (randomRIO)
import System.Random.Shuffle        (shuffleM)

import Server.Convert
import Server.Hardware
import Server.LmdbStore
import Server.Process
import Server.Time
import Server.Types.Logging
import Server.Types.Machine
import Server.Util

import qualified Data.Heap     as H
import qualified Data.IntMap   as IM
import qualified Data.Map      as M
import qualified Data.Sequence as Q
import qualified Data.Set      as S
import qualified Data.Vector   as V
import qualified Plun          as P

-- -----------------------------------------------------------------------

-- | The `mainloop` is a large, stateful computation. `Machine` is the state
-- type of that (and by extension) the majority of StateT functions in this
-- file.
--
-- A large part of the state here is keeping track of `Request` state. One
-- pattern you'll see is a map of "open" requests, which are requests waiting
-- for something, while "ready" requests are for things which are completed
-- but haven't been executed yet.
--
-- Most of the rest are lookup tables for quick access.
data Machine = MACHINE {
  machineName                  :: MachineName,

  -- The size of the random process id to generate. Incremented on collision.
  machineNextProcessBits       :: Nat,

  -- Processes which we persist in LogBatches.
  machineProcesses             :: IntMap Process,

  -- Request Queuing ----------------------------------------------------------

  -- Sends which are to non-existent processes. Nothing in the formalism stops
  -- this from happening. We must handle it or else we get into weird states in
  -- the entirely possible case where we randomly pick a targeted processid.
  machineOpenSends             :: Map RequestHandle (ProcessId, Fan),

  -- Sends where the target process exists and we can perform a recv.
  machineReadySendToRecv       :: Map RequestHandle (ProcessId, Fan),

  -- Sends where we executed a recv on the target and can now tell the sender
  --
  -- You cannot combine this and `machineReadySendToRecv` into one step because
  -- running the recv could cause the send to cancel, and you can't detect that
  -- without looking at removed requests.
  machineSendToAck             :: Map RequestHandle (ProcessId, Fan),

  -- Sends by process id. Used for quick lookups of what is and isn't valid
  -- when a process is created / dies.
  machineSendsByDstProcessId   :: Map ProcessId (Set RequestHandle),

  -- Open forks.
  machineReadyForks            :: Map RequestHandle Fan,

  -- Hardware -----------------------------------------------------------------

  machineCallIdFreelist        :: IntFreelist,

  machineCallIdToReqHandle     :: IntMap RequestHandle,
  machineReqHandleToCallId     :: Map RequestHandle Int,

  -- | Provided function which routes calls to hardware.
  machineHardwareCall          :: RouterFun,

  machineHardwareResponseQueue :: TQueue (Int, Fan),

  -- Logging -------------------------------------------------------------------
  machineNextBatchNum          :: Nat,

  machineLastSnapshot          :: BatchNum,

  machineLmdbThread            :: LmdbThread,

  -- Input ---------------------------------------------------------------------
  machineMachineQueue          :: TQueue MachineEvent
  }

data Work
  = MachineCmd MachineEvent
  | DoResponse RequestHandle Response
  | DoCallResponse Int Fan
  deriving (Show)

makeFieldLabels ''Machine

newMachine
    :: TQueue MachineEvent
    -> RouterFun
    -> LmdbThread
    -> MachineName
    -> IO Machine
newMachine mq routerFun lmdbt tn = do
  respQ <- newTQueueIO
  pure MACHINE {
    machineName = tn,
    machineNextProcessBits = 16,
    machineProcesses = mempty,
    machineOpenSends = mempty,
    machineReadySendToRecv = mempty,
    machineSendToAck = mempty,
    machineSendsByDstProcessId = mempty,
    machineReadyForks = mempty,
    machineCallIdFreelist = emptyIntFreelist,
    machineCallIdToReqHandle = mempty,
    machineReqHandleToCallId = mempty,
    machineHardwareCall = routerFun,
    machineHardwareResponseQueue = respQ,
    machineNextBatchNum = 0,
    machineLastSnapshot = BatchNum 0,
    machineLmdbThread = lmdbt,
    machineMachineQueue = mq
    }

getNextProcessId :: StateT Machine IO ProcessId
getNextProcessId = do
  bits <- use #nextProcessBits
  addr <- fromInteger <$> (liftIO $ randomRIO (0, fromIntegral $ 2 ^ bits))

  processes <- use #processes
  case member addr processes of
    True  -> retry
    False -> pure $ ProcessId addr
 where
   retry = do
     modifying' #nextProcessBits (+1)
     getNextProcessId

getNextBatchNum :: StateT Machine IO BatchNum
getNextBatchNum = do
  bn <- use #nextBatchNum
  modifying #nextBatchNum (+1)
  pure $ BatchNum bn

-- -----------------------------------------------------------------------

-- | Builds a SimpleMachine which will interact externally with a given
-- `Router`, load its state from a given `LmdbStore` referred to by
-- `LmdbThread` wrapper, and
replayMachine :: RouterFun
              -> LmdbThread
              -> ReplayFrom
              -> MachineName
              -> IO MachineHandle
replayMachine routerFun lmdbt replayFrom tn = do
  thControlQ <- newTQueueIO
  machine <- newMachine thControlQ routerFun lmdbt tn
  thAsync <- async $ evalStateT start machine
  pure $ MachineHandle{..}
  where
    start :: StateT Machine IO ()
    start = do
      q <- liftIO $ newTBMQueueIO 100
      liftIO $ atomically $ queueReplayFrom lmdbt tn replayFrom q
      readUntilClosed True q

    readUntilClosed :: Bool -> TBMQueue LogBatch -> StateT Machine IO ()
    readUntilClosed first q = do
      batch <- (liftIO $ atomically $ readTBMQueue q)
      --putStrLn $ tshow batch
      case batch of
        Nothing -> do
          putStrLn $ "<< Completed Replay >>"
          mainloop
        Just lb -> replayBatch first lb >> readUntilClosed False q

    replayBatch :: Bool -> LogBatch -> StateT Machine IO ()
    replayBatch True lb =
      -- Ensure we're starting from a valid place.
      case (batchNum lb, snapshot lb) of
        (BatchNum 0, _) -> do
          -- We're replaying from the 0th LogBatch, and must replay all of
          -- history.
          replayBatch False lb
        (BatchNum num, Just ss) -> do
          -- We're replaying from a LogBatch which has a snapshot, so load that
          -- into state.
          changesets <- loadSnapshot ss

          forM_ changesets $ \rc -> do
            forM_ (rc ^. #addedRequests) addRequest

          replayBatch False lb
        _ -> error "Invalid LogBatch. Can't resume from here."

    replayBatch False LogBatch{batchNum, executed} = do
      assign' #nextBatchNum (1 + unBatchNum batchNum)
      forM_ executed $ \receipt -> do
        applyChangeset =<< case receipt of
          ri@ReceiptInit{..} -> do
            traceM $ "ReceiptInit: " ++ (show ri)
            (changeset, process) <- liftIO $
              buildInitialProcess initPid initVal
            modifying' #processes (pidInsert initPid process)
            pure changeset

          ReceiptFork{..} -> do
            let handle = RequestHandle forkReqPid forkReqIdx
            use #readyForks <&> lookup handle >>= \case
              Nothing -> error "No matching fork request?"
              Just v  -> do
                doRunResponse handle (RespFork forkAssignedPid v)

          ReceiptVal{..}  ->
            doRunResponse (RequestHandle receiptPid receiptIdx)
                          (RunValue receiptVal)

          ReceiptRecv{..} -> do
            -- When there's a recv in the log, both the sender and the receiver
            -- were logged.
            let senderHandle = RequestHandle recvSendPid recvSendIdx
            val <- use (#readySendToRecv % at senderHandle)
            case val of
              Nothing -> error "No matching send for recv during replay."
              Just (pid, val) -> do
                let recvHandle = RequestHandle recvPid recvIdx
                let recv = RespRecv senderHandle val
                doRunResponse recvHandle recv

    doRunResponse handle req = runResponse handle req >>= \case
      Nothing    -> error "Empty response during replay."
      Just exeCs -> pure exeCs

-- | The main execution loop for the SimpleMachine. This is the main entry
-- point to a running machine.
--
-- SimpleMachine is single threaded, so mainloop takes a piece of work from
-- `getNextWork`, and switches what it does based on the current request. For
-- local computation, this is applying a `Response` to an outstanding request,
-- which follows this form:
--
-- - Try to `runResponse`.
--
-- - If running it succeeded, try to `buildReceipt` and
--   `syncWriteLogBatchOfOneReceipt` if this event has to write a receipt to
--   the log.
--
-- - `applyChangeset` to perform all the side effects.
--
mainloop :: StateT Machine IO ()
mainloop = do
  putStrLn "About to getNextWork"
  w <- getNextWork
  putStrLn "----------------"
  putStrLn $ "Performing " ++ (tshow w)
  case w of
    MachineCmd MachineEventSnapshot -> do
      performSnapshot
      mainloop
    MachineCmd MachineEventSnapshotAndShutdown -> do
      traceM "MachineEventSnapshotAndShutdown"
      performSnapshot
    MachineCmd MachineEventImmediateShutdown -> pure ()
    DoResponse reqH resp -> execResponse reqH resp
    DoCallResponse callid val -> execCallResponse callid val
  where
    execResponse reqH resp = do
      mybChangeset <- runResponse reqH resp
      case mybChangeset of
        Nothing -> do
          putStrLn "Nothing!"
          mainloop
        Just changeset -> do
          buildReceipt changeset >>= \case
            Nothing      -> pure ()
            Just receipt -> syncWriteLogBatchOfOneReceipt receipt
          applyChangeset changeset
          mainloop

    execCallResponse callid val = do
      zoom #callIdFreelist (freelistPush callid)

      m <- use #callIdToReqHandle
      case lookup callid m of
        Nothing   -> do
          traceM "Bad callid"
          mainloop
        Just reqH -> do
          modifying' #callIdToReqHandle (deleteMap callid)
          execResponse reqH (RunValue val)


-- | Write the current state of all `machineProcesses` to a LogBatch in
-- the event log.
--
-- Snapshot writing in SimpleMachine is synchronous and does not try to write
-- a snapshot while executing additional events.
performSnapshot :: StateT Machine IO ()
performSnapshot = do
    bn <- getNextBatchNum
    ss <- saveSnapshot
    buildLogBatch bn (Just ss) [] >>= blockOnWriteLogBatch
    assign #lastSnapshot bn

-- | Returns the next piece of work for the `mainloop` to perform, blocking if
-- there's no current work.
--
-- `getNextWork` will always prioritize incoming commands to the machine itself
-- (snapshot, shutdown, etc). When there's no work to be done, this function
-- blocks until work is available.
getNextWork :: StateT Machine IO Work
getNextWork = do
    -- Randomize the order in which we try to handle work for fairness reasons.
    receivers <- shuffleM [ getReadySendToRecv
                          , getSendToAck
                          , getForks
                          , tryGetCallResponse
                          ]
    let order = [ tryGetMachineCmd ] ++ receivers ++ [ waitForAction ]

    runFirst order >>= \case
      Just x  -> pure x
      Nothing -> error "waitForAction returned no work!?"
  where
    runFirst [] = pure Nothing
    runFirst (fun : xs) = do
      fun >>= \case
        Nothing -> runFirst xs
        Just x  -> pure $ Just x

    check label fun = do
      m <- use label
      case M.null m of
        True  -> pure $ Nothing
        False -> do
          (reqH, sendRep) <- randomEntry m
          (target, resp) <- fun reqH sendRep
          pure $ Just $ DoResponse target resp

    randomEntry m = do
      idx <- liftIO $ randomRIO (0, M.size m - 1)
      pure $ M.elemAt idx m

    tryGetMachineCmd = do
      mq <- use #machineQueue
      mybTe <- atomically $ tryReadTQueue mq
      case mybTe of
        Just x  -> pure $ Just $ MachineCmd $ x
        Nothing -> pure $ Nothing

    tryGetCallResponse = do
      hrq <- use #hardwareResponseQueue
      mybR <- atomically $ tryReadTQueue hrq
      case mybR of
        Just (callid, resp) -> pure $ Just $ DoCallResponse callid resp
        Nothing             -> pure $ Nothing

    getReadySendToRecv = check #readySendToRecv $ \reqH (pid, v) -> do
      pure $ (recvHandle pid, RespRecv reqH v)

    getSendToAck = check #sendToAck $ \reqH (pid, v) -> do
      pure $ (reqH, RunValue (NAT 0))

    getForks = check #readyForks $ \reqH v -> do
      pid <- getNextProcessId
      pure $ (reqH, RespFork pid v)

    waitForAction :: StateT Machine IO (Maybe Work)
    waitForAction = do
      use #processes <&> IM.null >>= \case
        True -> do
          -- Shutdown when all processes are gone.
          pure $ Just $ MachineCmd MachineEventImmediateShutdown
        False -> do
          machineQ <- use #machineQueue
          hardwareQ <- use #hardwareResponseQueue
          atomically $ (readMachineCommand machineQ <|>
                        readHardwareResponse hardwareQ)

    readMachineCommand mq = do
      readTQueue mq >>= (pure . Just . MachineCmd)

    readHardwareResponse hrq = do
      (callid, val) <- readTQueue hrq
      pure $ Just $ DoCallResponse callid val

-- | Runs a `Response` against a given `RequestHandle`.
--
-- This looks up the correct `Process` state, runs this Response, changes
-- state, and then gives an `ExecChangeset` back of the side effects we care
-- about.
runResponse :: RequestHandle -> Response
            -> StateT Machine IO (Maybe ExecChangeset)
runResponse rn@(RequestHandle pid _) resp =
 do
  putStrLn $ "<" ++ (tshow rn) ++ ">: " ++ (tshow resp)
  myb <- zoomMaybe (#processes % atP pid % _Just)
                   (execResponse rn resp)
  case myb of
    Just x  -> pure x
    Nothing -> error "No such process?!"
 where
  atP = at . unProcessId

-- | Given an `ExecChangeset` associated with one run of a `Response` via
-- `runResponse`, maybe produce a `Receipt` value which will go into a
-- `LogBatch`. Unlogged processes produce Nothing.
--
-- `Process` does not have enough information to build out a receipt: it can't
-- make the determination whether the send which was fed to the recv is still
-- alive.
buildReceipt :: ExecChangeset -> StateT Machine IO (Maybe Receipt)
buildReceipt cs = do
  let pid = cs ^. #processId
  pure $ case cs ^. #execEffect of
    -- If we try to build a receipt for an init, it's the first init.
    EEInit{..} -> Just $ ReceiptInit initVal initPid

    EEDefault{..} -> Just $ ReceiptVal pid execId execVal

    EEForked{..} -> Just $ ReceiptFork pid forkedRequestId
                                       (forkedProcess ^. #processId)

    EERecv{..} ->
      -- This implementation works because everything is synchronous. Once you
      -- try to do multithreading, you have to handle the case where the send
      -- was canceled while the recv was being handled.
      let (RequestHandle causePid cuaseReqid) = causingSend in
      Just $ ReceiptRecv pid recvRequestId causePid cuaseReqid

    -- When you crash, you don't write a receipt because it is as if nothing
    -- ever happened.
    EECrashed -> Nothing

-- | Writes a single receipt to the event log.
--
-- As stated in the Overview and in Caveats, SimpleMachine doesn't try to do
-- any event batching: it runs one response, writes the receipt for that one
-- response, and repeats. The only case where we have more than one receipt is
-- in the send/recv case.
syncWriteLogBatchOfOneReceipt :: Receipt -> StateT Machine IO ()
syncWriteLogBatchOfOneReceipt r = do
  bn <- getNextBatchNum
  buildLogBatch bn Nothing [r] >>= blockOnWriteLogBatch

-- Why do we take BatchNum? Because calling `getNextBatchNum` is an action
-- which increments, and performSnapshot needs to manipulate that separately.
buildLogBatch :: BatchNum
              -> Maybe Snapshot
              -> [Receipt]
              -> StateT Machine IO LogBatch
buildLogBatch bn ss r = LogBatch <$> pure bn
                                 <*> getNanoTime
                                 <*> use #lastSnapshot
                                 <*> pure ss
                                 <*> pure r

-- | Blocks processing until the log batch is committed to disk.
blockOnWriteLogBatch :: LogBatch -> StateT Machine IO ()
blockOnWriteLogBatch lb = do
  tn <- use #name
  lmdbt <- use #lmdbThread
  --putStrLn $ "Writing " ++ (tshow bn) ++ "..."
  sig <- newEmptyTMVarIO
  atomically $ queueWriteLogBatch lmdbt tn lb (putTMVar sig ())
  atomically $ takeTMVar sig
  --putStrLn $ "Log entry: " ++ (tshow lb)

-- | Process's `execResponse` produces an `ExecChangeset` which is all the
-- changes that a Process had while executing. This applies that changeset to
-- the Machine's state.
applyChangeset :: ExecChangeset -> StateT Machine IO ()
applyChangeset cs = do
  putStrLn $ "Changeset: " ++ (tshow cs)
  let csAid = cs ^. #processId

  case cs ^. #execEffect of
    EEForked{..} -> do
      when (forkedChangeset ^. #alive) $ do
        (registerForkedProcess forkedProcess forkedChangeset)

    -- When this Response was a recv, we can now queue up an acknowledgment to
    -- the causing send.
    EERecv{..} -> moveItem #readySendToRecv #sendToAck causingSend

    _            -> pure ()

  forM_ (cs ^. #removedRequests) $ \r -> do
    putStrLn $ "Removing request: " ++ (tshow r)
    removeRequest r
  forM_ (cs ^. #addedRequests) $ \r -> do
    putStrLn $ "Adding request: " ++ (tshow r)
    addRequest r

  when (not $ cs ^. #alive) unregisterProcess
  where
    registerForkedProcess s ec = do
      putStrLn "registerForkedProcess"
      let pid = processProcessId s
      modifying' #processes (pidInsert pid s)

      pids <- use (#sendsByDstProcessId % at pid % non' _Empty)
      forM_ pids (moveItem #openSends #readySendToRecv)

      applyChangeset ec

    unregisterProcess = do
      let pid = cs ^. #processId
      modifying' #processes (pidDelete pid)

      -- If this process died, ensure any outstanding ready sends to it get
      -- moved back to open.
      pids <- use (#sendsByDstProcessId % at pid % non' _Empty)
      forM_ pids (moveItem #readySendToRecv #openSends)

-- | Keeps track of a new `Request` being made by a `Process`.
addRequest :: (RequestHandle, Request) -> StateT Machine IO ()

addRequest (reqH@(RequestHandle pid reqid), ReqCall dst val) = do
  -- Associate a unique call identifier with this request, since this request
  -- could be canceled and then have the same request id assigned to it.
  callid <- zoom #callIdFreelist freelistGetNext
  modifying #callIdToReqHandle (insertMap callid reqH)
  modifying #reqHandleToCallId (insertMap reqH callid)
  respQ <- use #hardwareResponseQueue
  let cb = \x -> writeTQueue respQ (callid, x)

  name <- use #name
  hardwareCall <- use #hardwareCall
  atomically $ hardwareCall dst name pid val cb

addRequest (reqH, ReqFork v) = do
  modifying' (#readyForks) (M.insert reqH v)

addRequest (reqH, ReqSend pid val) = do
  modifying' (#sendsByDstProcessId % at pid % non' _Empty) (S.insert reqH)

  p <- use #processes
  let label = case member (unProcessId pid) p of
        True  -> #readySendToRecv
        False -> #openSends
  modifying label (M.insert reqH (pid, val))

addRequest (reqH, UNKNOWN v) =
  putStrLn $ "Warning: Unknown Request: " ++ (tshow v)

-- | Bookkeeping for when a `Process` stops making a `Request`.
removeRequest :: (RequestHandle, Request) -> StateT Machine IO ()

removeRequest (reqH, ReqCall dst val) = do
  use #reqHandleToCallId <&> M.lookup reqH >>= \case
    Nothing -> pure ()
    Just callid -> do
      -- Note: we don't release the callid here, only when we get a
      -- notification back from the hardware.
      modifying #callIdToReqHandle (deleteMap callid)
      modifying #reqHandleToCallId (deleteMap reqH)

removeRequest (reqH, ReqFork v) = do
  modifying' (#readyForks) (M.delete reqH)

removeRequest (reqH, ReqSend pid val) = do
  modifying' (#sendsByDstProcessId % at pid % non' _Empty) (S.delete reqH)
  modifying' (#openSends) (M.delete reqH)
  modifying' (#readySendToRecv) (M.delete reqH)
  modifying' (#sendToAck) (M.delete reqH)

removeRequest (reqH, UNKNOWN _) = pure ()

-- | When we are replaying an event log, we often start by loading a `Snapshot`
-- of the raw Plunder values that make up the process state.
--
-- This recreates the `Process` state and produces `ReloadChangeset`s which
-- read the `Claim`s and `Response`s to Machine state.
loadSnapshot :: Snapshot
             -> StateT Machine IO [ReloadChangeset]
loadSnapshot (Snapshot m) = do
  (rcs, processes) <- foldM load ([], IM.empty) $ M.toList m
  assign' #processes processes
  pure rcs
  where
    load :: ([ReloadChangeset], IntMap Process) -> (Nat, P.Fan)
         -> StateT Machine IO ([ReloadChangeset], IntMap Process)
    load (cs, im) (npid, val) = do
      let pid = fromIntegral npid
      (c, p) <- liftIO $ reloadProcessSnapshot (ProcessId pid, val)
      pure (c:cs, IM.insert pid p im)

-- | Takes the plunder value of all `Process`es and writes it to a Row.
saveSnapshot :: StateT Machine IO Snapshot
saveSnapshot = do
  lp <- use #processes
  pure $ Snapshot $ M.fromList $ map process $ IM.toList lp
  where
    process :: (IM.Key, Process) -> (Nat, Fan)
    process (k, p) = (fromIntegral k, p ^. #noun)

-- -----------------------------------------------------------------------
-- Utilities
-- -----------------------------------------------------------------------

-- | Removes a key and returns it.
getAndRemove src key = do
  use src <&> lookup key >>= \case
    Nothing -> error "Map mismatch: failed to match"
    Just value -> do
      modifying' src (IM.delete key)
      pure value

pidInsert :: ProcessId -> v -> IntMap v -> IntMap v
pidInsert p v = IM.insert (unProcessId p)  v

pidDelete :: ProcessId -> IntMap v -> IntMap v
pidDelete = IM.delete . unProcessId

-- -----------------------------------------------------------------------
