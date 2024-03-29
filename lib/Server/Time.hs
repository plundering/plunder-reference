module Server.Time where

import Numeric.Natural
import PlunderPrelude

import Data.Time.Clock       (NominalDiffTime, nominalDiffTimeToSeconds,
                              secondsToNominalDiffTime)
import Data.Time.Clock.POSIX (POSIXTime, getPOSIXTime)

newtype NanoTime = NanoTime { unNanoNat :: Nat }
  deriving newtype (Show, Eq, Num, Ord, NFData)

nanosSinceEpoch :: POSIXTime -> NanoTime
nanosSinceEpoch = NanoTime . floor . (1e9 *) . nominalDiffTimeToSeconds

epochNanosToPOSIX :: NanoTime -> POSIXTime
epochNanosToPOSIX =
  secondsToNominalDiffTime . (/ 1e9) . fromIntegral . unNanoNat

getNanoTime :: MonadIO m => m NanoTime
getNanoTime = liftIO (nanosSinceEpoch <$> getPOSIXTime)

withCalcRuntime :: MonadIO m => m a -> m (NanoTime, a)
withCalcRuntime action = do
  startUs <- getNanoTime
  out <- action
  endUs <- getNanoTime
  pure (endUs - startUs, out)
