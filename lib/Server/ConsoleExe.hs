module Server.ConsoleExe (main) where

import PlunderPrelude

import Data.Acquire
import Options.Applicative
import System.Directory
import System.Posix.Resource
import System.Posix.Signals

-- TODO: This should go in a common location instead of directly reaching in or
-- copypasting it.
import Server.LmdbStore
import Server.SimpleMachine
import Server.Types.Logging
import Server.Types.Machine

import Plun.Print   (decodeBtc, encodeBtc)
import Loot.Backend (plunLoad)
import Sire.ReplExe (showPlun)

import Plun as P

--------------------------------------------------------------------------------

data RunType
  = RTBoot FilePath MachineName Text
  | RTReplay ReplayFrom FilePath MachineName

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
   <> command "replay"
      ( info (RTReplay <$>
              replayFromOption <*>
              strArgument (metavar "STORE" <> help "Use this datstore") <*>
              parseMachineName)
        $ progDesc "Replays the events in a machine.")

runInfo :: ParserInfo RunType
runInfo = info (runType <**> helper)
  (fullDesc
  <> progDesc "Let's run plunder."
  <> header "new-network - a test for running plunder machines" )


-- Initial test here. We create a store, create one machine in it, and then
-- write one artificial logbatch, and then read it back.
main = do
  -- Setup plunder interpreter state.
  writeIORef P.vShowPlun (pure . showPlun)
  modifyIORef' P.state \st -> st { P.stFast = P.jetMatch }

  ctrlCPressed <- newEmptyTMVarIO
  let onKillSignal = atomically $ putTMVar ctrlCPressed ()
  for_ [sigTERM, sigINT] $ \sig -> do
    installHandler sig (Catch onKillSignal) Nothing

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
              PLN _ (P.DAT (P.PIN P.P{pinItem})) -> pure pinItem
              _                                  -> error "Impossible"

            handle <- bootNewMachine lmdbt machineName val
            asyncFinishOrCtrlC handle ctrlCPressed

    RTReplay replayFrom datastorePath machineName -> do
      with (openDatastore datastorePath) $ \lmdbt -> do
        loadMachine lmdbt machineName >>= \case
          NewMachine -> error "Trying to replay a non existing machine"
          ExistingMachine bn -> do
            handle <- replayMachine lmdbt replayFrom machineName
            asyncFinishOrCtrlC handle ctrlCPressed

asyncFinishOrCtrlC :: MachineHandle -> TMVar () -> IO ()
asyncFinishOrCtrlC handle onCtrlC = do
  -- Either the machine thread shuts down, or we shut it down.
  ret <- atomically $
         (Left <$> waitSTM (thAsync handle) <|>
          Right <$> readTMVar onCtrlC)
  case ret of
    Left _ -> do
      -- When the machine has shut itself down, we don't have to cleanup.
      pure ()

    Right _ -> do
      -- We must instruct the machine to shutdown.
      atomically $ writeTQueue' (thControlQ handle)
                                MachineEventSnapshotAndShutdown

      -- Wait for the machine to clean up.
      wait (thAsync handle)
