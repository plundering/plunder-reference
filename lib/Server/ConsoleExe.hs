module Server.ConsoleExe (main) where

import PlunderPrelude

import Data.Acquire
import Options.Applicative
import System.Directory
import System.Posix.Resource
import System.Posix.Signals
import System.Random         (randomRIO)

import Loot.Backend         (plunLoad)
import Plun.Print           (decodeBtc, encodeBtc)
import Server.Hardware
import Server.Hardware.Rand
import Server.LmdbStore     (getMachineNames, loadMachine, openDatastore,
                             queueWriteLogBatch)
import Server.SimpleMachine (replayMachine)
import Server.Time          (getNanoTime)
import Server.Types.Logging
import Server.Types.Machine
import Sire.ReplExe         (showPlun)

import Plun as P hiding ((^))


--------------------------------------------------------------------------------

data RunType
    = RTBoot FilePath MachineName Text
    | RTRun ReplayFrom FilePath

parseMachineName :: Parser MachineName
parseMachineName =
  MachineName <$> strArgument (metavar "NAME" <> help "Use this machine name")

replayFromOption :: Parser ReplayFrom
replayFromOption = flag LatestSnapshot EarliestBatch
                     ( long "replay-all"
                    <> help "Replay log from beginning." )

runType :: Parser RunType
runType = subparser
    $ command "boot"
      ( info (RTBoot <$>
              strArgument (metavar "STORE" <> help "Use this datastore") <*>
              parseMachineName <*>
              strArgument (metavar "HASH" <> help "Boot using this hash"))
        $ progDesc "Boots a machine.")
   <> command "run"
      ( info (RTRun <$>
              replayFromOption <*>
              strArgument (metavar "STORE" <> help "Use this datstore"))
        $ progDesc "Replays the events in a machine.")

runInfo :: ParserInfo RunType
runInfo = info (runType <**> helper)
  (fullDesc
  <> progDesc "Let's run plunder."
  <> header "new-network - a test for running plunder machines" )


-- | Initial test here. We create a store, create one machine in it, and then
-- write one artificial logbatch, and then read it back.
main = do
  -- Setup plunder interpreter state.
  writeIORef P.vShowPlun (pure . showPlun)
  modifyIORef' P.state \st -> st { P.stFast = P.jetMatch }

  ctrlCPressed <- newEmptyTMVarIO
  let onKillSignal = atomically $ putTMVar ctrlCPressed ()
  for_ [sigTERM, sigINT] $ \sig -> do
    installHandler sig (Catch onKillSignal) Nothing

  -- TODO: To really exercise the multicase, boot has to just boot and "replay"
  -- has to become "run"

  execParser runInfo >>= \case
    RTBoot datastorePath machineName bootHash -> do
      with (openDatastore datastorePath) $ \lmdbt -> do
        loadMachine lmdbt machineName >>= \case
          ExistingMachine _ ->
            error $ "Trying to overwrite existing machine " ++ (show machineName)
          NewMachine -> do
            let haz = decodeBtc bootHash
            hom <- liftIO getHomeDirectory
            pVal <- plunLoad (hom <> "/.sire") haz

            -- TODO: For now, we're manually unwrapping the implicit snapshot
            -- pin. In the short to medium term, we probably want to move to
            -- dedicated pill files instead of reading from the repl cache
            -- instead.
            val <- case pVal of
              P.PIN P.P{pinItem} -> pure pinItem
              _                  -> error "Impossible"

            -- To boot a machine, we just write the initial value as a single
            -- Init to the log, and then exit.
            lb <- buildInitialLogBatch val

            sig <- newEmptyTMVarIO
            atomically $
              queueWriteLogBatch lmdbt machineName lb (putTMVar sig ())
            atomically $ takeTMVar sig

    RTRun replayFrom datastorePath  -> do
      with (openDatastore datastorePath) $ \lmdbt -> do
        -- Get a list of all machines in this datastore.
        machines <- getMachineNames lmdbt

        with createHardwareRand $ \hardwareFun -> do
          db <- buildHardwareDb [(0, hardwareFun)]

          handles <- forM machines $ \machineName -> do
            loadMachine lmdbt machineName >>= \case
              NewMachine -> error "Trying to replay a non existing machine"
              ExistingMachine bn -> do
                replayMachine (routerFun db) lmdbt replayFrom machineName

          asyncFinishOrCtrlC handles ctrlCPressed


buildInitialLogBatch :: Fan -> IO LogBatch
buildInitialLogBatch init = do
  writeTime <- getNanoTime

  firstPid <- ProcessId <$> randomRIO (0, 2 ^ 16)

  let batchNum = BatchNum 0
      lastSnapshot = BatchNum 0
      snapshot = Nothing
      executed = [ReceiptInit init firstPid]
  pure LogBatch{..}

asyncFinishOrCtrlC :: [MachineHandle] -> TMVar () -> IO ()
asyncFinishOrCtrlC handles onCtrlC = do
  -- Either the machine thread shuts down, or we shut it down.
  let asyncs = map thAsync handles

  ret <- atomically $
         (Left <$> (mapM_ waitSTM asyncs) <|>
          Right <$> readTMVar onCtrlC)
  case ret of
    Left _ -> do
      -- When the machine has shut itself down, we don't have to cleanup.
      pure ()

    Right _ -> do
      putStrLn "Beginning shutdown"

      -- We must instruct each machine in the datastore to shutdown.
      forM_ handles $ \handle -> do
        traceM "Shutting down a handle..."
        atomically $ writeTQueue' (thControlQ handle)
                                  MachineEventSnapshotAndShutdown

      -- Wait for the machine to clean up.
      forM_ handles $ \handle -> do
        traceM "Waiting on a handle"
        wait (thAsync handle)
        traceM "Done!"

      putStrLn "Finished asyncFinishOrCtrlC"
