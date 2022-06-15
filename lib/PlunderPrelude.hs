module PlunderPrelude
    ( module X
    , writeTQueue'
    , writeTBQueue'
    )
where

import ClassyPrelude        as X
import Control.Monad.Except as X (MonadError(..), liftEither)
import Data.Coerce          as X (coerce)
import Data.Function        as X ((&))
import Data.List.NonEmpty   as X (NonEmpty(..))
import Natty                as X
import Optics               as X (_1, _2, assign, assign', at, modifying,
                                  modifying', over, to, use, view, (%), (^.))
import Optics.TH            as X
import System.IO.Unsafe     as X (unsafePerformIO)

writeTQueue' :: TQueue a -> a -> STM ()
writeTQueue' a b = writeTQueue a $! b

writeTBQueue' :: TBQueue a -> a -> STM ()
writeTBQueue' a b = writeTBQueue a $! b
