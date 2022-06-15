{-# OPTIONS_GHC -Wall   #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Werror #-}

module Jar.Nounable
    ( Hash256(..)
    , Nounable(..)
    , NounView(..)
    )
where

import PlunderPrelude hiding (hash, (%))

import Data.Hashable             (hash)
import Foreign.ForeignPtr        (withForeignPtr)
import Data.Bits                 (xor)
import Foreign.Ptr               (castPtr,)
import Foreign.Storable          (peek)

import Data.ByteString.Internal (ByteString(PS))


--------------------------------------------------------------------------------

class (Hashable a, Eq a) => Nounable a where
    type NounRef a
    mkCell   :: a -> a -> a
    mkAtom   :: Nat -> a
    mkRefr   :: NounRef a -> a
    nounView :: a -> NounView a

data NounView a
    = NATISH Nat
    | REFISH (NounRef a) Hash256
    | APPISH a a

--------------------------------------------------------------------------------

-- TODO Better show instance
newtype Hash256 = Hash256 ByteString
  deriving newtype (Eq, Ord, Show)
  deriving (Generic)

--------------------------------------------------------------------------------

{- | Default bytestring instance uses length, which is always 8 in
     this case.

     The bytestring is always at least 32 bytes wide, and the data is
     uniformly distributed.  Thus we simply cast the underlying pointer to
     (Ptr Int) and dereference.

     TODO Consider changing the representation of `pinHash` to four
          unboxed Word64s.
-}
instance Hashable Hash256 where
  hash (Hash256 (PS fpBuf _ _)) =
      unsafePerformIO $ withForeignPtr fpBuf
                      $ peek . castPtr
  {-# INLINE hash #-}

  hashWithSalt salt x = (salt * 16777619) `xor` (hash x)
  {-# INLINE hashWithSalt #-}
