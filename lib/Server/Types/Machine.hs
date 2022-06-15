module Server.Types.Machine where

-- Types used by all Machine implementations

import PlunderPrelude

data MachineEvent
  -- Have the machine take a snapshot.
  = MachineEventSnapshot
  -- Have the machine take a snapshot and exit its async.
  | MachineEventSnapshotAndShutdown
  -- Halt right now.
  | MachineEventImmediateShutdown
  deriving (Show)

data MachineHandle = MachineHandle {
  -- Sends messages to the Machine.
  thControlQ :: TQueue MachineEvent,

  -- Async for the machine thread. Exits on request with the shutdown
  -- MachineEvent.
  thAsync    :: Async ()
  }
