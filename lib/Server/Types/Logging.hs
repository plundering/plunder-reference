module Server.Types.Logging where

import PlunderPrelude

import Control.Concurrent.STM.TBMQueue
import Data.Vector
import Numeric.Natural                 (Natural)

import Plun (Pln)

-- A machine, a set of related processes, are given a human readable name.
newtype MachineName = MachineName String
  deriving newtype (Show, Eq, Ord)

-- Opaque process indexes. These are not visible from individual plunder
-- processes, but are stable referents to a processes. ProcessIdx written to
-- the event log are
newtype ProcessIdx = ProcessIdx { unProcessIdx :: Int }
  deriving newtype (Ord, Eq, Show)

-- Request Id is just a position into a returned array.
newtype RequestIdx = RequestIdx Int
  deriving newtype (Ord, Eq, Show)

data Receipt
  -- Receipt of initializing a new process. This doesn't run, but parses out a
  -- request row to cause future events to run.
  = ReceiptInit {
    initPidx :: ProcessIdx,
    initVal  :: Pln
    }

  -- Receipt of a %fork. This receipt indicates that we started a new process
  -- with the fork value specified at RequestNum. If the process is logged, we
  -- also record its assigned pidx, otherwise it doesn't matter and we don't.
  | ReceiptFork {
      forkReqPidx      :: ProcessIdx,
      forkReqIdx       :: RequestIdx,
      forkAssignedPidx :: Maybe ProcessIdx
      }

  -- Any request where we applied a raw value.
  | ReceiptVal {
      receiptPidx :: ProcessIdx,
      receiptIdx  :: RequestIdx,
      receiptVal  :: Pln
      }

  -- Local receive which can refer to an outstanding send request.
  | ReceiptRecv {
      recvPidx     :: ProcessIdx,
      recvIdx      :: RequestIdx,
      recvSendPidx :: ProcessIdx,
      recvSendIdx  :: RequestIdx
      }

  -- Receipt that we ran a kill request, so that during replay we also pause to
  -- stop and kill everything.
  | ReceiptKill {
      killPidxNotified :: ProcessIdx,
      killIdx          :: RequestIdx
      }
  deriving (Show)

-- Log batches count up from 0.
newtype BatchNum = BatchNum { unBatchNum :: Natural }
  deriving newtype (Eq, Show)

-- A snapshot is all the ship values implicitly identified by index, with 0
-- values representing unused indexes.
data Snapshot = Snapshot (Vector Pln)
  deriving (Show)

data LogBatch = LogBatch {
  -- Monotonically increasing id.
  batchNum     :: BatchNum,

  -- Time the LogBatch was written.
  writeTime    :: Natural,

  -- The batch number of the previous snapshot.
  lastSnapshot :: BatchNum,

  -- Not every log batch has a snapshot, but when they do, it is enough to
  -- entirely restart the system from this snapshot. It is safe to delete all
  -- prevoius LogBatches before a LogBatch with a snapshot.
  snapshot     :: Maybe Snapshot,

  -- Events which were run this batch. If there's a snapshot in this LogBatch,
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
  -- Play from the beginning.
  = EarliestBatch
  -- Replay from the latest marked snapshot.
  | LatestSnapshot
