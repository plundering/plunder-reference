module Server.Hardware where

-- The hardware

import PlunderPrelude

import Plun                 (Nat, Fan(NAT), (%%))
import Server.Types.Logging

import qualified StmContainers.Map as SM

data HardwareDb = HARDWARE {
  -- | All hardware
  hardwareList :: SM.Map Nat HardwareFun
  }

-- Processes can make CALLs to hardware. Right now, none of these requests are
-- cancellable; if you try to cancel one, it just won't give you a response.

-- | The type of the function that individual pieces of hardware must expose.
--
--
type HardwareFun = MachineName -> ProcessId -> Fan -> (Fan -> STM ())
                -> STM ()

-- | The hardware router which is exposed to each Machine. This is a superset
-- of HardwareFun and most implementations will pass most arguments verbatim.
type RouterFun = Nat -> MachineName -> ProcessId -> Fan -> (Fan -> STM ())
              -> STM ()

buildHardwareDb :: [(Nat, HardwareFun)] -> IO HardwareDb
buildHardwareDb inputs = do
  hardwareList <- SM.newIO
  atomically $ forM_ inputs $ \(i, hf) -> SM.insert hf i hardwareList
  pure $ HARDWARE{..}

routerFun :: HardwareDb -> RouterFun
routerFun HARDWARE{hardwareList} hardwareId srcName srcPid msg cb = do
  mybDst <- SM.lookup hardwareId hardwareList
  case mybDst of
    Nothing -> do
      -- In the case of unrecognized hardware, immediately send back a
      cb (NAT 0)

    Just fun -> do
      fun srcName srcPid msg cb
