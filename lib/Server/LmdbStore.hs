module Server.LmdbStore ( LmdbThread(lmdbThreadAsync)
                        , openDatastore
                        , loadMachine
                        , queueReplayFrom
                        , queueWriteLogBatch) where

import PlunderPrelude

import Control.Monad.State   (StateT, evalStateT)
import Data.Acquire
import Database.LMDB.Raw
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Ptr           (Ptr, castPtr, nullPtr)
import Foreign.Storable      (peek, poke, sizeOf)
import System.Directory

import Jar
import Server.Convert
import Server.Types.Logging

import qualified Data.ByteArray       as BA
import qualified Data.ByteString      as BS
import qualified Data.ByteString.UTF8 as BSU
import qualified Data.Map             as M
import qualified Plun                 as P

-- -----------------------------------------------------------------------
-- Low level interface
-- -----------------------------------------------------------------------

data LmdbStore = LmdbStore
  { lmdbStoreEnv                   :: MDB_env
  -- A map of tables where each table is the numeric kv
  , lmdbStoreMachineEventlogTables :: M.Map MachineName MachineLogTable
  -- List of all processes this machine contains.
  , lmdbStoreMachineTable          :: MDB_dbi
  -- A table of machine name to noun representation of a backup pointer.
  , lmdbStoreBackupsTable          :: MDB_dbi
  -- A mapping of noun hash to serialized noun representation.
  , lmdbStorePinCushion            :: MDB_dbi
  }

-- A table mapping a numeric batch number to a noun hash which has the
data MachineLogTable = MachineLogTable
  { logTable   :: MDB_dbi
  , numBatches :: TVar Word64
  }

instance Show MachineLogTable where
  show MachineLogTable{..} =
    "MachineLogTable{" ++ (show $ unsafePerformIO $ readTVarIO numBatches) ++
    "}"

data StoreExn
  = BadMachineName MachineName
  | BadWriteMachineTable MachineName
  | BadIndexInEventlog MachineName
  | BadWriteLogBatch MachineName BatchNum
  | BadPinWrite ByteString
  | BadPinMissing ByteString [ByteString]
  | BadDependency ByteString
  | BadDependencyDecode ByteString DecodeErr
  | BadLogEntry MachineName
  | EmptyLogTable MachineName
  deriving Show
instance Exception StoreExn where

makeFieldLabels ''LmdbStore

openEnv :: MonadIO m => FilePath -> m MDB_env
openEnv dir = liftIO $ do
  env <- mdb_env_create
  mdb_env_set_maxdbs env 1024 -- TODO: Adjust this dynamically?
  mdb_env_set_mapsize env (1024 ^ 4)
  mdb_env_open env dir []
  pure env

openTables :: MDB_txn -> IO (MDB_dbi, MDB_dbi, MDB_dbi)
openTables txn =
  (,,) <$> mdb_dbi_open txn (Just "MACHINES") [MDB_CREATE]
       <*> mdb_dbi_open txn (Just "BACKUPS") [MDB_CREATE, MDB_INTEGERKEY]
       <*> mdb_dbi_open txn (Just "NOUNS") [MDB_CREATE]

logName :: MachineName -> Maybe String
logName (MachineName strName) = Just ("LOG-" ++ strName)

open :: FilePath -> IO LmdbStore
open dir = do
  createDirectoryIfMissing True dir
  env <- openEnv dir
  with (writeTxn env) $ \txn -> do
    (t, b, n) <- openTables txn
    machineTables <- M.fromList <$> readMachines txn t
    pure $ LmdbStore env machineTables t b n
  where
    readMachines txn s = (getAllMachines txn s) >>= (mapM (openMachineTable txn))

    openMachineTable txn tn = do
      tbl <- mdb_dbi_open txn (logName tn) [MDB_INTEGERKEY]
      batches <- getNumBatches tn txn tbl
      numBatches <- newTVarIO (batches + 1)
      pure $ (tn, MachineLogTable tbl numBatches)

close :: FilePath -> LmdbStore -> IO ()
close dir LmdbStore{..} = do
  mdb_dbi_close lmdbStoreEnv lmdbStoreMachineTable
  mdb_dbi_close lmdbStoreEnv lmdbStoreBackupsTable
  forM_ (M.toList lmdbStoreMachineEventlogTables) $
    \(_, s) -> mdb_dbi_close lmdbStoreEnv (logTable s)
  mdb_dbi_close lmdbStoreEnv lmdbStorePinCushion
  mdb_env_sync_flush lmdbStoreEnv
  mdb_env_close lmdbStoreEnv

-- -----------------------------------------------------------------------

getAllMachines :: MDB_txn -> MDB_dbi -> IO [MachineName]
getAllMachines txn machineTable = with (cursor txn machineTable) $ \cur ->
  forRange cur RSBegin $ \pKey pVal -> do
    key <- peek pKey >>= valToBS
    pure $ MachineName $ BSU.toString key

getNumBatches :: MachineName -> MDB_txn -> MDB_dbi -> IO Word64
getNumBatches tn txn logTable =
  with (cursor txn logTable) $ \cur ->
    withKVPtrs (MDB_val 0 nullPtr) (MDB_val 0 nullPtr) $ \k v -> do
      mdb_cursor_get MDB_LAST cur k v >>= \case
        False -> pure 0
        True  -> peek k >>= (parseEventLogKey tn)

loadMachineFromStore :: MachineName -> StateT LmdbStore IO LoadMachine
loadMachineFromStore machineName@(MachineName tnStr) = do
  logTables <- use #machineEventlogTables
  case M.lookup machineName logTables of
    Nothing                  -> startNewMachine
    Just MachineLogTable{..} -> machineExists numBatches
  where
    flags = compileWriteFlags [MDB_NOOVERWRITE]
    tnBS = BSU.fromString tnStr

    startNewMachine :: StateT LmdbStore IO LoadMachine
    startNewMachine = do
      env <- use #env
      table <- use #machineTable
      log <- liftIO $ with (writeTxn env) $ \txn -> do
        -- TODO: Attach a value to the key. Empty for now for iteration.
        writeKV flags txn table tnBS BS.empty (BadWriteMachineTable machineName)

        -- Build an empty table with that machine name.
        machineDb <- mdb_dbi_open txn (logName machineName)
                                  [MDB_CREATE, MDB_INTEGERKEY]
        numEvents <- newTVarIO 0
        pure $ MachineLogTable machineDb numEvents

      modifying #machineEventlogTables (M.insert machineName log)
      pure NewMachine

    machineExists numBatches = do
      nb <- atomically $ readTVar numBatches
      pure $ ExistingMachine $ BatchNum $ fromIntegral nb

loadLogBatches :: MachineName -> ReplayFrom -> TBMQueue LogBatch
               -> StateT LmdbStore IO ()
loadLogBatches who rf q = do
  logTables <- use #machineEventlogTables
  env <- use #env
  case M.lookup who logTables of
    Nothing                 -> throwIO (BadMachineName who)
    Just MachineLogTable{..} -> do
      pinCushion <- use #pinCushion
      liftIO $ with (readTxn env) $ loadBatches logTable pinCushion
  where
    loadBatches log pinCushion txn = do
      case rf of
        EarliestBatch  -> readRangeStarting log pinCushion txn RSBegin
        LatestSnapshot -> with (cursor txn log) $ \cur -> do
          findLastSnapshot pinCushion log txn cur

    readRangeStarting log pinCushion txn firstKey = do
      with (cursor txn log) $ \cur -> do
        forRange cur firstKey $ \k v -> do
          kBS <- peek k >>= valToBS
          vBS <- peek v >>= valToBS
          val <- loadNoun txn pinCushion kBS vBS
          case fromNoun val of
            Nothing -> error "Couldn't read noun"
            Just lb -> atomically $ writeTBMQueue q lb
        atomically $ closeTBMQueue q

    findLastSnapshot pinCushion log txn cur = do
      withKVPtrs (MDB_val 0 nullPtr) (MDB_val 0 nullPtr) $ \k v -> do
        found <- mdb_cursor_get MDB_LAST cur k v
        unless found $ throwIO (EmptyLogTable who)
        kBS <- peek k >>= valToBS
        vBS <- peek v >>= valToBS
        val <- loadNoun txn pinCushion kBS vBS
        case fromNoun val of
          Nothing -> throwIO (BadLogEntry who)
          Just lb -> case (snapshot lb) of
            Just _ -> do
              -- The last logbatch was a snapshot. Just return it.
              atomically $ do
                writeTBMQueue q lb
                closeTBMQueue q
            Nothing -> do
              readRangeStarting log pinCushion txn $
                RSAt (fromW64 $ fromIntegral $ unBatchNum $ lastSnapshot lb)

writeLogBatch :: MachineName -> LogBatch -> StateT LmdbStore IO ()
writeLogBatch who lb@LogBatch{..} = do
  logTables <- use #machineEventlogTables
  case M.lookup who logTables of
    Nothing -> throwIO (BadMachineName who)
    Just MachineLogTable{..} -> do
      (pinz, lbNoun) <- liftIO (jar $ toNoun lb)
      let lbHash = P.cryptographicIdentity pinz lbNoun

      batch <- atomically (readTVar numBatches)
      let next = batch + 1
      writeLogBatch batch logTable lbHash lbNoun pinz
      atomically $ writeTVar numBatches next
  where
    logFlags = compileWriteFlags [MDB_NOOVERWRITE]

    writeLogBatch curBatch logTable lbHash lbNoun pinz = do
      when (curBatch /= (fromIntegral $ unBatchNum batchNum)) $
        error $ "Database/Runtime batch number mismatch: database expects " ++
                (show curBatch) ++ " but received " ++
                (show $ unBatchNum $ batchNum)

      let rep = concat $ serializeRep pinz lbNoun

      env <- use #env
      pinCushion <- use #pinCushion
      liftIO $ with (writeTxn env) $ \txn -> do
        forM pinz (stickPinInCushion txn pinCushion)
        let key :: ByteString = (fromW64 curBatch)
        writeKV logFlags txn logTable key rep
                (BadWriteLogBatch who batchNum)

    stickPinInCushion :: MDB_txn -> MDB_dbi -> P.Pin -> IO ()
    stickPinInCushion txn pinCushion P.P{..} = do
      -- Here, we only want to write a pin if the pin isn't already stuck in
      -- the pin cushion.
      alreadyInCushion <- keyExists txn pinCushion pinHash
      unless alreadyInCushion $ do
        for_ pinRefs (stickPinInCushion txn pinCushion)

        let rep = concat $ serializeRep pinRefs pinBlob
        writeKVNoOverwrite logFlags txn pinCushion pinHash
                           rep
                           (BadPinWrite pinHash)

-- Given a name, find a pin in the pin cushion.
loadPin :: MDB_txn -> MDB_dbi -> ByteString -> IO P.Val
loadPin txn pinCushion = loop []
  where
    loop stack pinHashBS = getKey txn pinCushion pinHashBS >>= \case
      Nothing -> throwIO (BadPinMissing pinHashBS stack)
      Just val -> do
        let (deps, bytz) = deserializeRep val
        depPins <- forM deps $ loop (pinHashBS : stack)

        case capBS depPins bytz of
          Left err   -> throwIO (BadDependencyDecode pinHashBS err)
          Right valu -> evaluate (force $ P.mkPin valu)

-- Given a bytestring representing a serialized rep of a noun, load
-- dependencies from the pincushion and return the Val.
loadNoun :: MDB_txn -> MDB_dbi -> ByteString -> ByteString -> IO P.Val
loadNoun txn pinCushion kBS vBS = do
  let (deps, bytz) = deserializeRep vBS
  refs <- forM deps (loadPin txn pinCushion)
  case capBS refs bytz of
    Left err   -> throwIO (BadDependencyDecode kBS err)
    Right valu -> evaluate (force valu)

serializeRep :: Vector P.Pin -> ByteString -> [ByteString]
serializeRep refs body = toList
  (fmap P.pinHash refs) <> [replicate 32 0, body]

deserializeRep :: ByteString -> (Vector ByteString, ByteString)
deserializeRep = loop []
  where
    loop xs val =
      let (key, rest) = splitAt 32 val in
      if | key == replicate 32 0 -> (fromList $ reverse xs, rest)
         | null key              -> error "Malformed rep in deserializeRep"
         | otherwise             -> loop (key:xs) rest

-- -----------------------------------------------------------------------

readTxn :: MDB_env -> Acquire MDB_txn
readTxn env = mkAcquire (mdb_txn_begin env Nothing True) mdb_txn_abort

writeTxn :: MDB_env -> Acquire MDB_txn
writeTxn env = mkAcquireType begin end
  where
    begin = mdb_txn_begin env Nothing False
    end txn = \case
      ReleaseException -> mdb_txn_abort  txn
      _                -> mdb_txn_commit txn

cursor :: MDB_txn -> MDB_dbi -> Acquire MDB_cursor
cursor txn dbi = mkAcquire (mdb_cursor_open txn dbi) mdb_cursor_close

byteArrayAsMdbVal :: BA.ByteArrayAccess ba => ba -> (MDB_val -> IO a) -> IO a
byteArrayAsMdbVal ba fun =
  BA.withByteArray ba $ \ptr ->
    fun (MDB_val (fromIntegral (BA.length ba)) (castPtr ptr))

keyExists :: BA.ByteArrayAccess a => MDB_txn -> MDB_dbi -> a -> IO Bool
keyExists txn db key =
  byteArrayAsMdbVal key $ \mKey ->
    isJust <$> mdb_get txn db mKey

getKey :: BA.ByteArrayAccess a => MDB_txn -> MDB_dbi -> a
       -> IO (Maybe ByteString)
getKey txn db key = byteArrayAsMdbVal key $ \mKey ->
  mdb_get txn db mKey >>= traverse valToBS

writeKV :: (BA.ByteArrayAccess a, BA.ByteArrayAccess b)
        => MDB_WriteFlags -> MDB_txn -> MDB_dbi
        -> a
        -> b
        -> StoreExn
        -> IO ()
writeKV flags txn db k v exn = do
  byteArrayAsMdbVal k $ \mKey ->
    byteArrayAsMdbVal v $ \mVal ->
      mdb_put flags txn db mKey mVal >>= \case
        True  -> pure ()
        False -> throwIO exn

-- Like writeKV, but don't throw an exception or
writeKVNoOverwrite :: (BA.ByteArrayAccess a, BA.ByteArrayAccess b)
                   => MDB_WriteFlags -> MDB_txn -> MDB_dbi
                   -> a
                   -> b
                   -> StoreExn
                   -> IO ()
writeKVNoOverwrite flags txn db k v exn = do
  (keyExists txn db k) >>= \case
    True  -> pure ()
    False -> writeKV flags txn db k v exn

data RangeStart a
  = RSBegin
  | RSAt ByteString

-- Given a numeric key range starting at (the beginning/a specific number),
-- iterate until the end, passing each kv pair to an action.
forRange :: MDB_cursor -> RangeStart a -> (Ptr MDB_val -> Ptr MDB_val -> IO b)
         -> IO [b]
forRange cur rs cb = do
  case rs of
    RSBegin -> do
      withKVPtrs (MDB_val 0 nullPtr) (MDB_val 0 nullPtr) $ \k v -> do
        mdb_cursor_get MDB_FIRST cur k v >>= \case
          False -> pure []
          True -> do
            x <- cb k v
            continue cur k v [x]
    RSAt start ->
      withContentsOf start $ \k ->
        withNullPtr $ \v ->
          mdb_cursor_get MDB_SET_KEY cur k v >>= \case
            False -> pure []
            True -> do
              x <- cb k v
              continue cur k v [x]
  where
    continue cur k v xs = do
      mdb_cursor_get MDB_NEXT cur k v >>= \case
        False -> pure $ reverse xs
        True -> do
          x <- cb k v
          continue cur k v (x:xs)

setCursorTo :: MDB_cursor -> MDB_cursor_op -> StoreExn -> IO ()
setCursorTo cur op exn = do
  withKVPtrs (MDB_val 0 nullPtr) (MDB_val 0 nullPtr) $ \k v -> do
    mdb_cursor_get op cur k v >>= \case
      False -> throwIO exn
      True  -> pure ()

valToBS :: MDB_val -> IO ByteString
valToBS (MDB_val sz ptr) = BS.packCStringLen (castPtr ptr, fromIntegral sz)

parseEventLogKey :: MachineName -> MDB_val -> IO Word64
parseEventLogKey tn (MDB_val sz ptr) =
  if sz == 8
  then peek (castPtr ptr)
  else throwIO (BadIndexInEventlog tn)

fromW64 :: (BA.ByteArray ba) => Word64 -> ba
fromW64 n = BA.allocAndFreeze 8 $ \p -> poke p n

withContentsOf :: BA.ByteArrayAccess ba => ba -> (Ptr MDB_val -> IO a) -> IO a
withContentsOf ba fn =
  BA.withByteArray ba $ \ptr -> do
    let item = MDB_val (fromIntegral (BA.length ba)) (castPtr ptr)
    allocaBytes (sizeOf item) $ \pK -> do
      poke pK item
      fn pK

withNullPtr :: (Ptr MDB_val -> IO a) -> IO a
withNullPtr fn = do
  let nul = MDB_val 0 nullPtr
  allocaBytes (sizeOf nul) $ \pK -> do
    poke pK nul
    fn pK


-- -----------------------------------------------------------------------
-- User level interaction
-- -----------------------------------------------------------------------

data LmdbThread = LmdbThread {
  lmdbThreadEvent :: TQueue LoggingEvent,
  lmdbThreadAsync :: Async ()
  }

data LoggingEvent
  -- Tries loading a machine. If the machine doesn't exist on disk, allocates
  -- structures and returns NewMachine, otherwise returns the latest BatchNum.
  = LogLoadMachine MachineName (LoadMachine -> STM ())

  -- Read and stream logbatches into the passed in queue. Closes the queue when
  -- done.
  | LogReplayFrom MachineName ReplayFrom (TBMQueue LogBatch)

  -- Writes a logbatch for a specific machine.
  | LogWriteBatch MachineName LogBatch (STM ())

  -- Exits the logging thread.
  | LogShutdown

openDatastore :: FilePath -> Acquire LmdbThread
openDatastore dir = mkAcquire start stop
  where
    start = do
      q <- newTQueueIO
      a <- asyncBound $ writerThread q (open dir)
      pure $ LmdbThread q a

    stop (LmdbThread q a) = do
      atomically $ writeTQueue' q LogShutdown
      wait a

loadMachine :: LmdbThread -> MachineName -> IO LoadMachine
loadMachine LmdbThread{..} machine = do
  cb <- newEmptyTMVarIO
  atomically $ writeTQueue' lmdbThreadEvent $
    LogLoadMachine machine (putTMVar cb)
  atomically $ takeTMVar cb

queueReplayFrom :: LmdbThread -> MachineName -> ReplayFrom -> TBMQueue LogBatch
                -> STM ()
queueReplayFrom LmdbThread{..} tn rf q =
  writeTQueue' lmdbThreadEvent (LogReplayFrom tn rf q)

queueWriteLogBatch :: LmdbThread -> MachineName -> LogBatch -> STM ()
                   -> STM ()
queueWriteLogBatch LmdbThread{..} tn lb cb =
  writeTQueue' lmdbThreadEvent (LogWriteBatch tn lb cb)

writerThread :: TQueue LoggingEvent -> IO LmdbStore -> IO ()
writerThread dbWriteEvent mkDbStore = handle onErr $ do
    db <- mkDbStore
    evalStateT loop db
  where
    onErr e = do
      putStrLn $ "db thread was killed by: " ++ (pack $ displayException e)
      throwIO (e :: SomeException)

    loop = (atomically $ readTQueue dbWriteEvent) >>= \case
      LogLoadMachine machine cb -> do
        machineState <- loadMachineFromStore machine
        atomically $ cb machineState
        loop

      LogReplayFrom machine rf q -> do
        loadLogBatches machine rf q
        loop

      LogWriteBatch machine b cb -> do
        writeLogBatch machine b
        atomically cb
        loop

      LogShutdown -> pure ()
