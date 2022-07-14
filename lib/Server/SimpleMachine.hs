module Server.SimpleMachine where

-- This is the simplest Truope. It is meant to be a simplification for teaching
-- and not an optimized implementation for production use, though it should
-- always produce the same results. This implementation blocks on everything.
--
-- This simplified version:
--
-- - Does absolutely no parallelism between processes. This is entirely
--   single-threaded, running all processes one at a time. The production
--   version has every process as an async, but that complicates the code quite
--   a bit.
--
-- - Pauses the world to take a snapshot. The production version has a ton of
--   complexity around keeping asynchronous processes processing events while a
--   coherent snapshot of the system is being taken. Unlike the production
--   version, this just stops the world.
--
-- - We also stop the world entirely when writing the event log to disk. This
--   simplifies response handling. The production version immediately releases
--   response that can't be observed outside of the Machine.
--
-- - We run only one event per log batch. This simplifies response queuing
--   logic, which otherwise has to keep track of what responses are queued for
--   what requests. The production version runs as many events as it can to
--   stuff into a log batch while waiting on the disk to commit the previous
--   log batch.
--
-- - Implements no real scheduling. In an attempt at fairness, this uses a
--   simplified random ordering of outstanding possible responses. The
--   production version prioritizes some responses over others for smoothness
--   and queuing reasons.

import PlunderPrelude

import Control.Concurrent.STM.Delay
import Control.Monad.State          (MonadState, StateT, evalStateT, get, gets,
                                     modify', put, runStateT)
import Crypto.Sign.Ed25519          (PublicKey(..), SecretKey(..),
                                     createKeypair, toPublicKey)
import Data.IntMap                  (IntMap)
import Numeric.Natural              (Natural)
import Optics
import Optics.At
import Optics.State.Operators
import Optics.TH
import Optics.Zoom
import Plun                         (Nat, Pln, pattern AT)
import Plun.Print                   (encodeBtc)
import System.Entropy               (getEntropy)
import System.Random                (randomRIO)
import System.Random.Shuffle        (shuffleM)

import Server.Convert
import Server.LmdbStore
import Server.Process
import Server.Time
import Server.Types.Logging
import Server.Types.Machine

import qualified Data.Heap     as H
import qualified Data.IntMap   as IM
import qualified Data.Map      as M
import qualified Data.Sequence as Q
import qualified Data.Set      as S
import qualified Data.Vector   as V
import qualified Plun          as P

-- -----------------------------------------------------------------------

secretToPublic :: Key -> Pub
secretToPublic = PUB . toPublicKey . unkey

appendSeq :: a -> Seq a -> Seq a
appendSeq a s = s |> a

deleteFirst :: Eq a => a -> Seq a -> Seq a
deleteFirst a s = case Q.findIndexL (== a) s of
  Nothing -> s
  Just i  -> Q.deleteAt i s

allM :: Monad m => (a -> m Bool) -> [a] -> m Bool
allM fun []     = pure True
allM fun (x:xs) = fun x >>= \case
  True  -> allM fun xs
  False -> pure False

-- -----------------------------------------------------------------------

type LocalToReqs = Map LocalAddress (Seq RequestHandle)

data Machine = MACHINE {
  machineName                       :: MachineName,

  machineNextLoggedPidx             :: Int,
  machineNextUnloggedPidx           :: Int,

  -- Logged process indexes which are free to be reused.
  machineLoggedFreelist             :: Seq ProcessIdx,

  -- Processes which we persist in LogBatches.
  machineLoggedProcesses            :: IntMap Process,

  -- Processes which are transient and go away.
  machineUnloggedProcesses          :: IntMap Process,

  -- Keys and Identity --------------------------------------------------------

  -- Currently used derived public keys based on secret key.
  machineEdClaimToPub               :: Map Key Pub,

  -- Index of which public key corresponds to which process. We don't want
  -- there to be multiple processes, but the model allows for it and there's no
  -- way to ban it in model. So we always send to the process with the lowest
  -- ProcessIdx for a pubkey. This metric is a) simple, b) deterministic on
  -- replay, c) usually not what the user would want to disincentivize users
  -- from having processes with multiple keys.
  machineEdPubToProcess             :: Map Pub (Set ProcessId),

  machineLocalClaimToProcess        :: Map LocalAddress (Set ProcessId),

  -- When an process asks for localNext, that address is put here temporarily
  -- before it's put in #localClaimToThread by the process.
  machineUnclaimedLocalAddressess   :: Set LocalAddress,

  machineNextLocalAddressBits       :: Nat,

  -- Request Queuing ----------------------------------------------------------

  --
  machineOpenNextRequests           :: Map RequestHandle (),

  -- Bury requests waiting on something to happen.
  machineOpenBurys                  :: Map RequestHandle LocalAddress,

  -- Bury requests which have fired but which have not been executed yet.
  machinePendingBurys               :: Map RequestHandle BuryWhy,

  -- Open bury requests by address being monitored.
  machineBuryByAddr                 :: Map LocalAddress (Set RequestHandle),

  -- A kill request receives a response once there are no processes,
  -- terminating any existing matching processes as necessary.
  machineOpenKills                  :: Map RequestHandle (LocalAddress, Pln),

  -- A list of sends which have successfully been delivered. A delivered send
  -- is not invalid or open. It was delivered and is valid, even if it becomes
  -- invalid in the future.
  machineDeliveredLocalSends        :: Map RequestHandle LocalSendRep,

  machineOpenLocalRecvs             :: Map RequestHandle LocalRecvRep,
  machineOpenLocalSends             :: Map RequestHandle LocalSendRep,

  -- All open, valid local sends by destination and source. Used for fast
  -- lookups.
  machineLocalSendQueue             :: Map LocalAddress LocalToReqs,

  machineLocalRecvsByAddr           :: Map LocalAddress (Set RequestHandle),

  -- A local send or recv is invalid if it's trying to send on or listen to a
  -- localaddress that isn't claimed. In sane programs, this should never
  -- happen. But this degenerate state must explicitly be handled as part of
  -- the formalism because nothing prevents it.
  machineInvalidLocalRecvs          :: Map RequestHandle LocalRecvRep,
  machineInvalidLocalRecvsByProcess :: Map ProcessId (Set RequestHandle),
  machineInvalidLocalSends          :: Map RequestHandle LocalSendRep,
  machineInvalidLocalSendsByProcess :: Map ProcessId (Set RequestHandle),

  -- Open forks.
  machineOpenLoggedForks            :: Map RequestHandle Pln,
  machineOpenUnloggedForks          :: Map RequestHandle Pln,

  -- All timers that should fire in the future, ordered as a heap.
  machineOpenTimers                 :: H.MinHeap (Natural, RequestHandle),

  -- Generate values
  machineOpenWhen                   :: Map RequestHandle (),
  machineOpenRand                   :: Map RequestHandle (),

  -- Logging -------------------------------------------------------------------
  machineNextBatchNum               :: Nat,

  machineLastSnapshot               :: BatchNum,

  machineLmdbThread                 :: LmdbThread,

  -- Input ---------------------------------------------------------------------
  machineMachineQueue               :: TQueue MachineEvent
  }

makeFieldLabels ''Machine

newMachine :: TQueue MachineEvent -> LmdbThread -> MachineName -> Machine
newMachine mq lmdbt tn = MACHINE {
  machineName = tn,
  machineNextLoggedPidx = 0,
  machineNextUnloggedPidx = 0,
  machineLoggedFreelist = mempty,
  machineLoggedProcesses = mempty,
  machineUnloggedProcesses = mempty,
  machineEdClaimToPub = mempty,
  machineEdPubToProcess = mempty,
  machineLocalClaimToProcess = mempty,
  machineUnclaimedLocalAddressess = mempty,
  machineNextLocalAddressBits = 16,
  machineOpenNextRequests = mempty,
  machineOpenBurys = mempty,
  machinePendingBurys = mempty,
  machineBuryByAddr = mempty,
  machineOpenKills = mempty,
  machineDeliveredLocalSends = mempty,
  machineOpenLocalRecvs = mempty,
  machineOpenLocalSends = mempty,
  machineLocalSendQueue = mempty,
  machineLocalRecvsByAddr = mempty,
  machineInvalidLocalRecvs = mempty,
  machineInvalidLocalRecvsByProcess = mempty,
  machineInvalidLocalSends = mempty,
  machineInvalidLocalSendsByProcess = mempty,
  machineOpenLoggedForks = mempty,
  machineOpenUnloggedForks = mempty,
  machineOpenTimers = mempty,
  machineOpenWhen = mempty,
  machineOpenRand = mempty,
  machineNextBatchNum = 0,
  machineLastSnapshot = BatchNum 0,
  machineLmdbThread = lmdbt,
  machineMachineQueue = mq
  }

getNextProcessIndex :: LogType -> StateT Machine IO ProcessIdx
getNextProcessIndex = \case
  Unlogged -> do
    num <- use #nextUnloggedPidx
    modifying #nextUnloggedPidx (+1)
    pure $ ProcessIdx num
  Logged -> do
    freelist <- use #loggedFreelist
    case freelist of
      Q.Empty -> do
        -- No holes to fill in the Snapshot array.
        num <- use #nextLoggedPidx
        modifying #nextLoggedPidx (+1)
        pure $ ProcessIdx num
      (x Q.:<| xs) -> do
        assign #loggedFreelist xs
        pure x

getNextProcessId :: LogType -> StateT Machine IO ProcessId
getNextProcessId lt = (ProcessId lt) <$> getNextProcessIndex lt

getNextBatchNum :: StateT Machine IO BatchNum
getNextBatchNum = do
  bn <- use #nextBatchNum
  modifying #nextBatchNum (+1)
  pure $ BatchNum bn

-- -----------------------------------------------------------------------

pidxInsert :: ProcessIdx -> v -> IntMap v -> IntMap v
pidxInsert p v = IM.insert (unProcessIdx p)  v

pidxDelete :: ProcessIdx -> IntMap v -> IntMap v
pidxDelete = IM.delete . unProcessIdx

bootNewMachine :: LmdbThread -> MachineName -> Pln -> IO MachineHandle
bootNewMachine lmdbt tn val = do
  thControlQ <- newTQueueIO
  thAsync <- async $ evalStateT start (newMachine thControlQ lmdbt tn)
  pure $ MachineHandle{..}
  where
    start = do
      pidx <- getNextProcessIndex Logged
      (changeset, process) <- liftIO $ buildInitialProcess pidx val
      modifying' #loggedProcesses (pidxInsert pidx process)

      buildReceipt changeset >>= \case
        Nothing      -> pure ()
        Just receipt -> syncWriteLogBatchOfOneReceipt receipt
      applyChangeset changeset

      mainloop

replayMachine :: LmdbThread -> ReplayFrom -> MachineName -> IO MachineHandle
replayMachine lmdbt replayFrom tn = do
  thControlQ <- newTQueueIO
  thAsync <- async $ evalStateT start (newMachine thControlQ lmdbt tn)
  pure $ MachineHandle{..}
  where
    start :: StateT Machine IO ()
    start = do
      q <- liftIO $ newTBMQueueIO 100
      liftIO $ atomically $ queueReplayFrom lmdbt tn replayFrom q
      readUntilClosed True q

    readUntilClosed :: Bool -> TBMQueue LogBatch -> StateT Machine IO ()
    readUntilClosed first q = do
      (liftIO $ atomically $ readTBMQueue q) >>= \case
        Nothing -> do
          putStrLn $ "<< Completed Replay >>"
          queueBuryMissings
          mainloop
        Just lb -> replayBatch first lb >> readUntilClosed False q

    asProcess :: ProcessIdx -> ProcessId
    asProcess = ProcessId Logged

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
          assign' #nextLoggedPidx $ snapshotNextProcessIndex ss

          changesets <- loadSnapshot ss

          -- Replay all key events before all request parsing because those
          -- could be referenced by existing requests.
          forM_ changesets $ \rc -> do
            forM_ (rc ^. #addedClaims) $ \claim -> do
              registerClaim (rc ^. #processId) claim

          forM_ changesets $ \rc -> do
            forM_ (rc ^. #addedRequests) addRequest

          replayBatch False lb
        _ -> error "Invalid LogBatch. Can't resume from here."

    replayBatch False LogBatch{batchNum, executed} = do
      assign' #nextBatchNum (1 + unBatchNum batchNum)
      forM_ executed $ \receipt -> do
        applyChangeset =<< case receipt of
          ReceiptInit{..} -> do
            (changeset, process) <- liftIO $
              buildInitialProcess initPidx initVal
            modifying' #loggedProcesses (pidxInsert initPidx process)
            pure changeset

          ReceiptFork{..} -> do
            let handle = RequestHandle (ProcessId Logged forkReqPidx) forkReqIdx
            case forkAssignedPidx of
              Nothing -> do
                use #openUnloggedForks <&> lookup handle >>= \case
                  Nothing -> error "No matching fork request?"
                  Just v  -> doRunResponse handle (Fork FOReplayUnlogged v)
              Just assignedPidx -> do
                modifying' #loggedFreelist (filter (/= assignedPidx))
                use #openLoggedForks <&> lookup handle >>= \case
                  Nothing -> error "No matching fork request?"
                  Just v -> do
                    let a = asProcess assignedPidx
                    doRunResponse handle (Fork (FOLogged a) v)

          ReceiptVal{..}  ->
            doRunResponse (RequestHandle (asProcess receiptPidx) receiptIdx)
                          (RunValue receiptVal)

          ReceiptRecv{..} -> do
            -- When there's a recv in the log, both the sender and the receiver
            -- were logged.
            let senderHandle = RequestHandle (asProcess recvSendPidx) recvSendIdx
                recvHandle = RequestHandle (asProcess recvPidx) recvIdx

            use #openLocalSends <&> lookup senderHandle >>= \case
              Nothing -> error "No matching send for recv during replay."
              Just LocalSendRep{..} -> do
                let recv = RecvLocal senderHandle
                                     (P.mkRow [toNoun srSrc, srVal]) srSrc srDst
                doRunResponse recvHandle recv

          ReceiptKill{..} -> do
            let handle = RequestHandle (asProcess killPidxNotified) killIdx
            use #openKills <&> lookup handle >>= \case
              Nothing -> error "No matching kill request?"
              Just (victimAddress, reason) -> do
                (loggedPidxs, allKillsets) <-
                  killAllProcessesMatching victimAddress
                doRunResponse handle (RunKill reason loggedPidxs allKillsets)

    doRunResponse handle req = runResponse handle req >>= \case
      Nothing    -> error "Empty response during replay."
      Just exeCs -> pure exeCs

    queueBuryMissings = do
      buryByAddr <- use #buryByAddr
      claimToProcess <- use #localClaimToProcess
      forM_ (M.keys buryByAddr) $ \addr -> do
        unless (M.member addr claimToProcess) $ do
          broadcastBury addr WhyMissing

    snapshotNextProcessIndex :: Snapshot -> Int
    snapshotNextProcessIndex (Snapshot v) =
      if V.null v
      then 0
      else V.length v + 1

mainloop :: StateT Machine IO ()
mainloop = do
  w <- getNextWork
  putStrLn "----------------"
  -- putStrLn $ "Performing " ++ (tshow w)
  case w of
    MachineCmd MachineEventSnapshot -> do
      performSnapshot
      mainloop
    MachineCmd MachineEventSnapshotAndShutdown ->
      performSnapshot
      -- shuts down by not recursing
    MachineCmd MachineEventImmediateShutdown -> pure ()
    DoResponse reqH resp -> execResponse reqH resp
  where
    execResponse reqH resp = do
      mybChangeset <- runResponse reqH resp
      case mybChangeset of
        Nothing -> mainloop
        Just changeset -> do
          buildReceipt changeset >>= \case
            Nothing      -> pure ()
            Just receipt -> syncWriteLogBatchOfOneReceipt receipt
          applyChangeset changeset
          mainloop

data Work
  = MachineCmd MachineEvent
  | DoResponse RequestHandle Response
  deriving (Show)

performSnapshot :: StateT Machine IO ()
performSnapshot = do
    tn <- use #name
    bn <- getNextBatchNum
    writeTime <- getNanoTime
    ls <- use #lastSnapshot
    ss <- saveSnapshot
    let lb = LogBatch bn writeTime ls (Just ss) []

    lmdbt <- use #lmdbThread

    --putStrLn $ "Writing snapshot " ++ (tshow bn) ++ "..."
    sig <- newEmptyTMVarIO
    atomically $ queueWriteLogBatch lmdbt tn lb (putTMVar sig ())
    atomically $ takeTMVar sig
    --putStrLn $ "Log entry: " ++ (tshow lb)

    assign #lastSnapshot bn

-- Returns the next piece of work to perform, blocking if there isn't one.
getNextWork :: StateT Machine IO Work -- (RequestNum, Response)
getNextWork = do
    -- Randomize the order in which we try to handle work for fairness reasons.
    receivers <- shuffleM [ getNext
                          , getDeliveredLocalSends
                          , getLoggedForks
                          , getUnloggedForks
                          , getKill
                          , getPendingBury
                          , getLocalSend
                          , getWhen
                          , getRand
                          , getTimer
                          -- TODO: getExternal
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
          resp <- fun sendRep
          pure $ Just $ DoResponse reqH resp

    randomEntry m = do
      idx <- liftIO $ randomRIO (0, M.size m - 1)
      pure $ M.elemAt idx m

    tryGetMachineCmd = do
      mq <- use #machineQueue
      mybTe <- atomically $ tryReadTQueue mq
      case mybTe of
        Just x  -> pure $ Just $ MachineCmd $ x
        Nothing -> pure $ Nothing

    getNext =
      check #openNextRequests $ \() -> (RunValue . AT) <$> getNextLocal

    getDeliveredLocalSends =
      check #deliveredLocalSends $ \sendRep -> pure $ responseOK

    getWhen =
      check #openWhen $ \() -> (RunValue . AT) <$> (liftIO getNanoTime)

    getRand =
      check #openRand $ \() -> (RunValue . P.mkBar) <$> (liftIO $ getEntropy 64)

    getKill =
      check #openKills $ \(victimAddress, reason) -> do
        -- TODO: Killing all the processes which match an address has to run as
        -- one atomic action. When I write the Production machine, performing a
        -- kill will have to pause the world: if you don't pause the world, a
        -- process could spawn another process with the same address
        -- asynchronously. (even though they shouldn't. the formalism doesn't
        -- prevent it.)
        (loggedPidxs, allKillsets) <- killAllProcessesMatching victimAddress
        pure (RunKill reason loggedPidxs allKillsets)

    getLocalSend = do
      openLocalRecvs <- use #openLocalRecvs
      randomRecvs <- shuffleM $ M.toList openLocalRecvs
      findFirst randomRecvs
      where
        -- For every local recv, try to fill it with a message from the local
        -- queue.
        findFirst [] = pure $ Nothing
        findFirst ((reqH, LocalRecvRep{..}) : xs) = do

          localSendQ <- use #localSendQueue
          let dstQ = fromMaybe mempty $ M.lookup rrDst localSendQ

--          putStrLn $ "Local send queue state: " ++ (tshow dstQ)
          let validSends = M.restrictKeys dstQ rrSrcs
--          putStrLn $ "Valid sends: " ++ (tshow validSends)

          if M.null validSends
          then findFirst xs
          else do
            (dst, seq) <- randomEntry validSends
            case seq of
              Empty -> error "empty queue in #localSendQueue"
              sendReq Q.:<| _ -> do
                openLocalSends <- use #openLocalSends
                case M.lookup sendReq openLocalSends of
                  Nothing -> error "uncancelled request in #localSendQueue"
                  Just LocalSendRep{..} -> do
                    pure $ Just $ DoResponse reqH $
                      RecvLocal sendReq (P.mkRow [toNoun srSrc, srVal])
                                srSrc srDst

    getLoggedForks = check #openLoggedForks $ \v -> do
      pid <- getNextProcessId Logged
      pure $ Fork (FOLogged pid) v

    getUnloggedForks = check #openUnloggedForks $ \v -> do
      pid <- getNextProcessId Unlogged
      pure $ Fork (FOUnlogged pid) v

    getPendingBury = check #pendingBurys $ \bw -> do
      pure $ RunValue $ toNoun bw

    getTimer = do
      now <- getNanoTime
      h <- use #openTimers
      case H.view h of
        Nothing -> pure $ Nothing
        Just ((wen, reqH), tail) -> do
          case now >= wen of
            False -> pure $ Nothing
            True -> do
              -- (We don't modify the timer stack here, we modify it when the
              -- process remove the request.)
              pure $ Just (DoResponse reqH $ RunValue $ AT now)


    waitForAction :: StateT Machine IO (Maybe Work)
    waitForAction = do
      {- TODO: Must check unlogged processes too. -}
      use #loggedProcesses <&> IM.null >>= \case
        True -> do
          -- Shutdown when all processes are gone.
          pure $ Just $ MachineCmd MachineEventImmediateShutdown
        False -> do
          h <- use #openTimers
          wakeup <- case H.viewHead h of
            Nothing -> do
              pure $ Nothing
            Just (wen, reqH) -> do
              now <- getNanoTime
              let delayAmountInNs = max 0 (fromIntegral $ wen - now)
              let delayAmountInMs = delayAmountInNs `div` 1000
              d <- liftIO (newDelay delayAmountInMs)
              pure $ Just (reqH, d)

          mq <- use #machineQueue
          putStrLn "Beginning waitForAction waiting..."
          liftIO $ case wakeup of
            Just (reqH, delay) -> do
              atomically $ do
                (waitDelay delay >>
                 (pure $ Just $ DoResponse reqH responseOK)) <|>
                  (readTQueue mq >>= (pure . Just . MachineCmd))
            Nothing -> ((Just . MachineCmd) <$> (atomically $ readTQueue mq))


getNextLocal :: StateT Machine IO Nat
getNextLocal = do
  bits <- use #nextLocalAddressBits
  addr <- fromInteger <$> (liftIO $ randomRIO (0, fromIntegral $ 2 ^ bits))

  generated <- use #unclaimedLocalAddressess
  case member (LOCAL addr) generated of
    True -> do
      modifying' #nextLocalAddressBits (+1)
      getNextLocal
    False -> do
      active <- use #localClaimToProcess
      case M.lookup (LOCAL addr) active of
        Just _ -> do
          modifying' #nextLocalAddressBits (+1)
          getNextLocal
        Nothing -> do
          modifying' #unclaimedLocalAddressess (S.insert (LOCAL addr))
          pure addr

atP = at . unProcessIdx

runResponse :: RequestHandle -> Response
            -> StateT Machine IO (Maybe ExecChangeset)
runResponse rn@(RequestHandle pid _) resp = do
  putStrLn $ "<" ++ (tshow rn) ++ ">: " ++ (tshow resp)
  let label = case pid of
        ProcessId Unlogged pidx -> (#unloggedProcesses % atP pidx % _Just)
        ProcessId Logged pidx   -> (#loggedProcesses % atP pidx % _Just)
  join <$> zoomMaybe label (execResponse rn resp)

-- The process does not have enough information to build out a receipt: it can't
-- make the determination whether the send which was fed to the recv is still
-- alive.
buildReceipt :: ExecChangeset -> StateT Machine IO (Maybe Receipt)
buildReceipt cs = case cs ^. #processId of
  ProcessId Unlogged _      -> pure $ Nothing
  ProcessId Logged threadId -> case cs ^. #execEffect of
    EEInit{..} -> case initAid of
      -- If we try to build a receipt for an init, it's the first init.
      ProcessId Logged pidx -> pure $ Just $ ReceiptInit pidx initVal
      ProcessId Unlogged _ ->
        error "Attempted to initialize log with an unlogged process!"
    EEDefault{..} -> pure $ Just $
      ReceiptVal threadId execIdx execVal
    EEForked{..} -> pure $ Just $ ReceiptFork threadId forkedRequestIdx $
      case (processProcessId forkedProcess) of
        ProcessId Unlogged _          -> Nothing
        ProcessId Logged assignedPidx -> Just assignedPidx
    EELocalRecv{..} -> do
      -- When we build a receipt for a local send, we want to be able to
      -- point to the local send in the list of the list of outstanding
      -- events. But if we can't (because the send request was canceled), we
      -- can fallback to the raw value.
      mybOpen <- use #openLocalSends <&> lookup causingSend
      let (RequestHandle causeAid causeReqIdx) = causingSend
      case (mybOpen, causeAid) of
        (Just _, ProcessId Logged causePidx) ->
          -- The sender was logged and is still open so it can be referred to.
          pure $ Just $ ReceiptRecv threadId recvRequestIdx causePidx causeReqIdx
        (_, _) ->
          pure $ Just $ ReceiptVal threadId recvRequestIdx recvVal
    EEKilled{..} -> do
      pure $ Just $ ReceiptKill threadId killRequestIdx
    EECrashed -> do
      -- When you crash, you don't write a receipt because it is as if nothing
      -- ever happened.
      pure $ Nothing

syncWriteLogBatchOfOneReceipt :: Receipt -> StateT Machine IO ()
syncWriteLogBatchOfOneReceipt r = do
  tn <- use #name
  bn <- getNextBatchNum
  ls <- use #lastSnapshot
  writeTime <- getNanoTime
  let lb = LogBatch bn writeTime ls Nothing [r]

  lmdbt <- use #lmdbThread

  --putStrLn $ "Writing " ++ (tshow bn) ++ "..."
  sig <- newEmptyTMVarIO
  atomically $ queueWriteLogBatch lmdbt tn lb (putTMVar sig ())
  atomically $ takeTMVar sig
  --putStrLn $ "Log entry: " ++ (tshow lb)

-- argh. ExecChangeset{..} is shadowing the accessors.
applyChangeset :: ExecChangeset -> StateT Machine IO ()
applyChangeset cs = do
--  putStrLn $ "Changeset: " ++ (tshow cs)
  let csAid = cs ^. #processId

  case cs ^. #execEffect of
    -- We must process the claim and state of a fork first, so that its claims
    -- are registered to a new thread id before the fork response in case the
    EEForked{..} -> when (forkedChangeset ^. #alive == StillAlive) $ do
      (registerForkedProcess forkedProcess forkedChangeset)

    -- When this Response was a recv, we can now queue up an acknowledgment to
    -- the causing send.
    EELocalRecv{..} -> do
      -- Grab the previous state.
      openSends <- use #openLocalSends

      modifying' (#openLocalSends) (M.delete causingSend)
      deleteLocalSend recvTo recvFrom causingSend

      -- TODO: OK, what should be done about deleting the causing send? If we
      -- have

      -- If this send was still open when we received, then queue a response to
      -- the send.
      case M.lookup causingSend openSends of
        Nothing -> do
          putStrLn "WARNING: causing send missing. This might be an error."
          putStrLn $ "Machine state: " ++ (tshow openSends)
        Just sr -> do
          modifying' (#deliveredLocalSends) (M.insert causingSend sr)
    EEKilled{..} -> do
      forM_ killKilledChangesets $ \kcs -> do
        let killedAid = kcs ^. #processId
        forM_ (kcs ^. #removedClaims) $ \c ->
          --putStrLn $ "Unregistering claim (killed): " ++ (tshow c)
          unregisterClaim killedAid c (Just $ WhyKilled killReason)
        forM_ (kcs ^. #removedRequests) $ \r ->
          --putStrLn $ "Removing request (killed): " ++ (tshow r)
          removeRequest r
    _            -> pure ()

  let mybBuryWhy = case cs ^. #alive of
        StillAlive       -> Nothing
        Shutdown         -> Just $ WhyShutdown
        MemoryExhaustion -> Just $ WhyMemory
        Timeout          -> Just $ WhyTimeout

  forM_ (cs ^. #addedClaims) $ \c -> do
    putStrLn $ "Registering claim: " ++ (tshow c)
    registerClaim csAid c

  forM_ (cs ^. #removedClaims) $ \c -> do
    putStrLn $ "Unregistering claim: " ++ (tshow c)
    unregisterClaim csAid c mybBuryWhy

  forM_ (cs ^. #removedRequests) $ \r -> do
    putStrLn $ "Removing request: " ++ (tshow r)
    removeRequest r
  forM_ (cs ^. #addedRequests) $ \r -> do
    putStrLn $ "Adding request: " ++ (tshow r)
    addRequest r

  case mybBuryWhy of
    Nothing -> pure ()
    Just buryWhy -> do
      -- The process has halted. All the claim and request cleanup happened
      -- above, but we need to clean up the process separately.
      putStrLn $ "Cleaning up after process " ++ (tshow csAid)
      case csAid of
        ProcessId Unlogged idx -> do
          modifying' #unloggedProcesses (pidxDelete idx)
        ProcessId Logged idx   -> do
          modifying' #loggedProcesses (pidxDelete idx)
          modifying' #loggedFreelist (appendSeq idx)
  where
    registerForkedProcess s ec = do
      let (ProcessId lt pidx) = processProcessId s
          label = case lt of
            Logged   -> #loggedProcesses
            Unlogged -> #unloggedProcesses
      modifying' label (pidxInsert pidx s)
      applyChangeset ec

-- TODO: Next up: dealing with claims. claims are the next big deal.

registerClaim :: ProcessId -> Claim -> StateT Machine IO ()

registerClaim pid (LocalClaim i) = do
  modifying' #unclaimedLocalAddressess (S.delete i)
  modifying' (#localClaimToProcess % at i % non' _Empty) (S.insert pid)

  -- For every invalid recv from this thread, if we registered the address that
  -- made it invalid, we have to make it valid.
  invalidRecvByProcess <- use #invalidLocalRecvsByProcess
  case lookup pid invalidRecvByProcess of
    Nothing   -> pure ()
    Just reqs -> do
      localRecvs <- use #invalidLocalRecvs
      let (nowValid, stillInvalid) =
            partition (isRecvValid localRecvs) $ S.toList reqs
      forM_ nowValid (moveItem #invalidLocalRecvs #openLocalRecvs)
      assign' (#invalidLocalRecvsByProcess % at pid % non' _Empty) $
        S.fromList stillInvalid

  invalidSendByProcess <- use #invalidLocalSendsByProcess
  case lookup pid invalidSendByProcess of
    Nothing -> pure ()
    Just reqs -> do
      localSends <- use #invalidLocalSends
      let (nowValid, stillInvalid) =
            partition (isSendValid localSends) $ S.toList reqs
      forM_ nowValid (moveItem #invalidLocalSends #openLocalSends)
      assign' (#invalidLocalSendsByProcess % at pid % non' _Empty) $
        S.fromList stillInvalid
  where
    isRecvValid localRecvs rh = case lookup rh localRecvs of
      Nothing -> error "#invalidLocalRecvs and #invalidLocalRecvsByPidx disagree"
      Just LocalRecvRep{rrDst} -> rrDst == i

    isSendValid localSends rh = case lookup rh localSends of
      Nothing -> error "#invalidLocalSends and #invalidLocalSendsByPidx disagree"
      Just LocalSendRep{srSrc} -> srSrc == i


registerClaim pid (Ed25519Blake3Claim k) = do
  edClaimToPub <- use #edClaimToPub
  case lookup k edClaimToPub of
    Just pub  ->
      modifying' (#edPubToProcess % at pub % _Just) (S.insert pid)
    Nothing -> do
      let pub = secretToPublic k
      modifying' (#edClaimToPub) (M.insert k pub)
      modifying' (#edPubToProcess) (M.insert pub (S.singleton pid))

  -- TODO: Do something like the LocalClaim moving invalid claim case.

unregisterClaim :: ProcessId -> Claim -> Maybe BuryWhy -> StateT Machine IO ()

unregisterClaim pid (LocalClaim i) buryWhy = do
  modifying' (#localClaimToProcess % at i % non' _Empty) (S.delete pid)

  stillValidClaims <- (member i) <$> use #localClaimToProcess
  unless stillValidClaims $ do
    -- Clean up all sends from the queue
    srcs <- (fromMaybe mempty . lookup i) <$> use #localSendQueue
    modifying #localSendQueue (M.delete i)
    forM_ (M.toList srcs) $ \(_, toClean) ->
      forM_ toClean $ \reqH ->
        moveItem #openLocalSends #invalidLocalSends reqH

    reqs <- (fromMaybe mempty . lookup i) <$> use #localRecvsByAddr
    modifying #localSendQueue (M.delete i)
    forM_ reqs $ \reqH -> do
      moveItem #openLocalRecvs #invalidLocalRecvs reqH

    -- If we released the key when the process is not shutting down, we have to
    -- broadcast a special buryWhy to the rest of the
    broadcastBury i $ case buryWhy of
      Nothing  -> WhyMissing
      Just why -> why

unregisterClaim pid (Ed25519Blake3Claim k) _ = do
  edClaimToPub <- use #edClaimToPub
  case lookup k edClaimToPub of
    Nothing  -> error "Process tried to unregister a non-existent claim"
    Just pub -> do
      modifying' (#edPubToProcess % at pub % non' _Empty) (S.delete pid)
      error "TODO: Need to do real cleanup here when implementing remotes."

addRequest :: (RequestHandle, Request) -> StateT Machine IO ()

addRequest (reqH, LocalNext) = do
  modifying' (#openNextRequests) (M.insert reqH ())

addRequest (reqH, ForkLogged v) = do
  modifying' (#openLoggedForks) (M.insert reqH v)

addRequest (reqH, ForkUnlogged v) = do
  modifying' (#openUnloggedForks) (M.insert reqH v)

addRequest (reqH, LocalKill addr reason) = do
  modifying' (#openKills) (M.insert reqH (addr, reason))

addRequest (reqH, LocalBury addr) = do
  modifying' (#openBurys) (M.insert reqH addr)
  modifying' (#buryByAddr % at addr % non' _Empty) (S.insert reqH)

addRequest (reqH@(RequestHandle pid _), LocalSend rep@LocalSendRep{..}) = do
  doesProcessClaimLocal pid srSrc >>= \case
    False -> do
      modifying' (#invalidLocalSends) (M.insert reqH rep)
      modifying' (#invalidLocalSendsByProcess % at pid % non' _Empty)
                 (S.insert reqH)
    True  -> do
      modifying' (#openLocalSends) (M.insert reqH rep)
      modifying' (#localSendQueue % at srDst % non' _Empty %
                  at srSrc  % non' _Empty)
                 (appendSeq reqH)

addRequest (reqH@(RequestHandle pid _), LocalRecv rep@LocalRecvRep{..}) = do
  doesProcessClaimLocal pid rrDst >>= \case
    False -> do
      modifying' (#invalidLocalRecvs) (M.insert reqH rep)
      modifying' (#invalidLocalRecvsByProcess % at pid % non' _Empty)
                 (S.insert reqH)
    True  -> do
      modifying' (#openLocalRecvs) (M.insert reqH rep)
      modifying' (#localRecvsByAddr % at rrDst % non' _Empty) (S.insert reqH)

addRequest (reqH, Wait wen) = do
  modifying' (#openTimers) (H.insert (wen, reqH))

addRequets (reqH, When) = do
  modifying' (#openWhen) (M.insert reqH ())

addRequets (reqH, Rand) = do
  modifying' (#openRand) (M.insert reqH ())

removeRequest :: (RequestHandle, Request) -> StateT Machine IO ()

removeRequest (reqH, LocalNext) = do
  modifying' (#openNextRequests) (M.delete reqH)

removeRequest (reqH, ForkLogged v) = do
  modifying' (#openLoggedForks) (M.delete reqH)

removeRequest (reqH, ForkUnlogged v) = do
  modifying' (#openUnloggedForks) (M.delete reqH)

removeRequest (reqH, LocalKill addr reason) = do
  modifying' (#openKills) (M.delete reqH)

removeRequest (reqH, LocalBury addr) = do
  modifying' (#openBurys) (M.delete reqH)
  modifying' (#pendingBurys) (M.delete reqH)
  modifying' (#buryByAddr % at addr % non' _Empty) (S.delete reqH)

removeRequest (reqH@(RequestHandle pid _), LocalSend rep@LocalSendRep{..}) = do
  modifying' (#deliveredLocalSends) (M.delete reqH)
  modifying' (#invalidLocalSends) (M.delete reqH)
  modifying' (#invalidLocalSendsByProcess % at pid % non' _Empty) (S.delete reqH)
  modifying' (#openLocalSends) (M.delete reqH)
  deleteLocalSend srDst srSrc reqH

removeRequest (reqH@(RequestHandle pid _), LocalRecv rep@LocalRecvRep{..}) = do
  modifying' (#invalidLocalRecvs) (M.delete reqH)
  modifying' (#invalidLocalRecvsByProcess % at pid % non' _Empty) (S.delete reqH)
  modifying' (#openLocalRecvs) (M.delete reqH)
  modifying' (#localRecvsByAddr % at rrDst % non' _Empty) (S.delete reqH)

removeRequest (reqH, Wait wen) = do
  modifying' (#openTimers) removeItem
  where
    -- TODO: Haskell's Data.Heap doesn't allow removal of specific
    -- elements, when that should be a O(log n) method? The common case is
    -- removing the head of the heap so optimize that O(1), but fall back
    -- to a linear filter scan if not.
    removeItem h = case H.view h of
      Nothing                           -> h
      Just (i, tail) | i == (wen, reqH) -> tail
      Just _                            -> H.filter (== (wen, reqH)) h

removeRequets (reqH, When) = do
  modifying' (#openWhen) (M.delete reqH)

removeRequets (reqH, Rand) = do
  modifying' (#openRand) (M.delete reqH)

killAllProcessesMatching :: LocalAddress
                         -> StateT Machine IO ([ProcessIdx], [KilledChangeset])
killAllProcessesMatching victim = do
  -- Get the processes to kill and remove their claims.
  processIds <- (#localClaimToProcess % at victim % non' _Empty) <<.= S.empty

  let (loggedIds, unloggedIds) = partitionEithers $
                                 fmap processIdToProcessIdx $
                                 S.toList processIds

  logged   <- forM loggedIds $
    \(ProcessIdx idx) -> (getAndRemove #loggedProcesses idx)
  unlogged <- forM unloggedIds $
    \(ProcessIdx idx) -> (getAndRemove #unloggedProcesses idx)

  loggedDelsets <- liftIO $ mapM killProcess logged
  unloggedDelsets <- liftIO $ mapM killProcess unlogged

  pure (loggedIds, loggedDelsets ++ unloggedDelsets)

broadcastBury :: LocalAddress -> BuryWhy -> StateT Machine IO ()
broadcastBury la bw = do
  byAddr <- use #buryByAddr
  modifying' #buryByAddr (M.delete la)
  case lookup la byAddr of
    Nothing -> pure ()
    Just reqs -> forM_ reqs $ \reqH -> do
      modifying' #openBurys (M.delete reqH)
      modifying' #pendingBurys (M.insert reqH bw)


loadSnapshot :: Snapshot
             -> StateT Machine IO [ReloadChangeset]
loadSnapshot (Snapshot v) = do
  (rcs, logged) <- foldM load ([], IM.empty) $ zip [0..] $ V.toList v
  assign' #loggedProcesses logged
  pure rcs
  where
    load :: ([ReloadChangeset], IntMap Process) -> (Int, Pln)
         -> StateT Machine IO ([ReloadChangeset], IntMap Process)
    load (cs, im) (idx, val) = case val of
      AT 0 -> do
        modifying' #loggedFreelist (appendSeq $ ProcessIdx idx)
        pure (cs, im)
      _ -> do
        (c, p) <- liftIO $ reloadProcessSnapshot (ProcessIdx idx, val)
        pure (c:cs, IM.insert idx p im)


saveSnapshot :: StateT Machine IO Snapshot
saveSnapshot = do
  lp <- use #loggedProcesses
  let size = case IM.toDescList lp of
        []      -> 0
        (s,_):_ -> s
  pure $ Snapshot $ V.generate size (fromIntmap lp)
  where
    fromIntmap lp idx = case lookup idx lp of
      Nothing      -> AT 0
      Just process -> process ^. #noun


deleteLocalSend :: LocalAddress -> LocalAddress -> RequestHandle
                -> StateT Machine IO ()
deleteLocalSend dst src reqH = do
  modifying' (#localSendQueue % at dst % non' _Empty % at src % non' _Empty)
             (deleteFirst reqH)

doesProcessClaimLocal :: ProcessId -> LocalAddress -> StateT Machine IO Bool
doesProcessClaimLocal pid la = do
  claims <- use #localClaimToProcess
  case lookup la claims of
    Nothing   -> pure $ False
    Just pids -> pure $ member pid pids

{- moveItem #srcMap #dstMap key -}
moveItem :: (MonadState s m,
             Is k1 A_Setter,
             Is k2 A_Setter,
             Is k2 A_Getter,
             Ord k3)
         => Optic' k2 is1 s (Map k3 a)
         -> Optic k1 is2 s s (Map k3 a) (Map k3 a)
         -> k3
         -> m ()
moveItem src dst key = do
  use src <&> lookup key >>= \case
    Nothing -> pure ()
    Just value -> do
      modifying' src (M.delete key)
      modifying' dst (M.insert key value)

getAndRemove src key = do
  use src <&> lookup key >>= \case
    Nothing -> error "Map mismatch: failed to match"
    Just value -> do
      modifying' src (IM.delete key)
      pure value

-- -----------------------------------------------------------------------
