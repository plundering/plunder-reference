module Server.Hardware.Rand where

import PlunderPrelude

import Data.Acquire
import System.Entropy (getEntropy)

import Plun.Eval
import Server.Hardware


createHardwareRand :: Acquire HardwareFun
createHardwareRand = do
  inQ <- newTBQueueIO 100
  mkAcquire (start inQ) stop
  pure $ \m pid pln cb -> writeTBQueue inQ (m, pid, pln, cb)
  where
    start inQ = async (randThread inQ)
    stop = cancel

    randThread inQ = forever $ do
      (_, _, v, cb) <- atomically $ readTBQueue inQ
      traceM "In Rand: "
      case v of
        (NAT n) -> do
          eny <- BAR <$> (getEntropy $ fromIntegral n)
          atomically $ cb $ NAT 0 %% eny

        -- Ignore any other requests
        _      -> pure ()
