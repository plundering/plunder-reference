module Server.Types.Logging where

import PlunderPrelude

import Control.Concurrent.STM.TBMQueue
import Data.Vector
import Numeric.Natural                 (Natural)

import Plun (Fan)

-- | A machine, a set of related processes, are given a human readable name.
newtype MachineName = MachineName Text
  deriving newtype (Show, Eq, Ord)

-- | Process identifier. Unique inside a machine.
newtype ProcessId = ProcessId { unProcessId :: Int }
  deriving newtype (Ord, Eq, Show, Hashable)

-- | Request Id is either 0 (indicating the implicit recv) or a one-based index
-- into the array of Requests that a Process has as it's first partially
-- applied parameter.
newtype RequestId = RequestId Int
  deriving newtype (Ord, Eq, Show)

-- | A record of one call to a Process and its side effects.
data Receipt
  -- | Receipt of initializing a new process. This doesn't run, but parses out a
  -- request row to cause future events to run.
  = ReceiptInit {
    initVal :: Fan,
    initPid :: ProcessId
    }

  -- | Receipt of a %fork. This receipt indicates that we started a new process
  -- with the fork value specified at RequestNum. If the process is logged, we
  -- also record its assigned pidx, otherwise it doesn't matter and we don't.
  | ReceiptFork {
      forkReqPid      :: ProcessId,
      forkReqIdx      :: RequestId,
      forkAssignedPid :: ProcessId
      }

  -- | Any request where we applied a raw value.
  | ReceiptVal {
      receiptPid :: ProcessId,
      receiptIdx :: RequestId,
      receiptVal :: Fan
      }

  -- | Local receive which can refer to an outstanding send request.
  | ReceiptRecv {
      recvPid     :: ProcessId,
      recvIdx     :: RequestId,
      recvSendPid :: ProcessId,
      recvSendIdx :: RequestId
      }
  deriving (Show)

-- | Log batches count up from 0.
newtype BatchNum = BatchNum { unBatchNum :: Natural }
  deriving newtype (Eq, Show)

-- | A snapshot is all the ship values implicitly identified by index, with 0
-- values representing unused indexes.
data Snapshot = Snapshot (Map Nat Fan)
  deriving (Show)

-- | The atomic unit written to the event log. A Plunder Machine should be able
-- to be restored from a sequence of LogBatches which are an execution record.
data LogBatch = LogBatch {
  -- | Monotonically increasing id.
  batchNum     :: BatchNum,

  -- | Time the LogBatch was written.
  writeTime    :: Natural,

  -- | The batch number of the previous snapshot.
  --
  -- Since event logs are a map from a `BatchNum` to a `LogBatch`, this is used
  -- to quickly rewind backwards through the event log to the previous
  -- snapshot.
  lastSnapshot :: BatchNum,

  -- | Not every log batch has a snapshot, but when they do, it is enough to
  -- entirely restart the system from this snapshot. It is safe to delete all
  -- prevoius LogBatches before a LogBatch with a snapshot.
  snapshot     :: Maybe Snapshot,

  -- | Events which were run this batch. If there's a snapshot in this LogBatch,
  -- these are run on top of the snapshot state.
  executed     :: [Receipt]
  }
  deriving (Show)

-- -----------------------------------------------------------------------

-- Interface to the logging system.

data LoadMachine
  = NewMachine
  | ExistingMachine BatchNum

data ReplayFrom
  -- | Play from the beginning.
  = EarliestBatch
  -- | Replay from the latest marked snapshot.
  | LatestSnapshot
