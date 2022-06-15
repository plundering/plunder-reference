module Server.Time where

import PlunderPrelude

import Data.Time.Clock       (NominalDiffTime, nominalDiffTimeToSeconds,
                              secondsToNominalDiffTime)
import Data.Time.Clock.POSIX (POSIXTime, getPOSIXTime)
import Numeric.Natural

nanosSinceEpoch :: POSIXTime -> Natural
nanosSinceEpoch = floor . (1e9 *) . nominalDiffTimeToSeconds

epochNanosToPOSIX :: Natural -> POSIXTime
epochNanosToPOSIX = secondsToNominalDiffTime . (/ 1e9) . fromIntegral

getNanoTime :: MonadIO m => m Natural
getNanoTime = liftIO (nanosSinceEpoch <$> getPOSIXTime)
