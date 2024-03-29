{-# LANGUAGE NoFieldSelectors    #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wall        #-}
{-# OPTIONS_GHC -Werror      #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Fan.Types
    ( Fan(..)
    , PrimopCrash(..)
    , Nat
    , Pin(..)
    , DagInfo(..)
    , Law(..)
    , LawName(..)
    , Rex
    , setExec
    , toNat
    , mkCab
    , mkCow
    , Prog(..)
    , Run(..)
    , Hash256
    )
where

import PlunderPrelude hiding (hash, (^))

import Jelly.Types (Hash256)
import Rex         (GRex)

import qualified Data.Map             as M
import qualified Data.Set             as S
import qualified Data.Vector.Storable as SV

-- Types -----------------------------------------------------------------------

data PrimopCrash = PRIMOP_CRASH !Nat !Fan

{-
    Note that `code` is lazy.  We don't "compile" it until it is
    first run.
-}
data Law = L
    { name :: !LawName
    , args :: !Nat
    , body :: !Fan
    , code :: Prog
    }

-- All data already forced upon construction.
instance NFData Law where rnf = \L{} -> ()

newtype LawName = LN { nat :: Nat }
  deriving newtype (Eq, Ord, NFData)

data DagInfo = DAG_INFO
    { hash :: !Hash256
    , refs :: !(Vector Pin)
    }

{-
    -   `hash` is the BLAKE3 hash of the concatenation of the jelly head
        and jelly body.

    -   `refs` is all the pins that we reference, in noun-traversal order.
        This corresponds directly to the pin length and order used in Jelly
        save.

     TODO Use on-heap data for hash and blob (ShortByteString, or similar).
-}
data Pin = P
    { node :: IORef (Maybe DagInfo) -- Cryptographic Hash and edge-list
    , quik :: Int                   -- Quick Hash
    , args :: !Nat
    , item :: !Fan
    , exec :: SmallArray Fan -> Fan
    }

-- All data already forced upon construction.
instance NFData Pin where rnf = \P{} -> ()

setExec :: (SmallArray Fan -> Fan) -> Pin -> Pin
setExec x (P n q a i _) = P n q a i x

data Fan
    = NAT !Nat
    | PIN !Pin
    | FUN !Law
    | KLO !Int {-# UNPACK #-} !(SmallArray Fan)
    | BAR !ByteString
    | ROW !(Vector Fan)
    | TAB !(Map Fan Fan)
    | CAb !(Set Fan)
    | COw !Nat
    | REX !Rex
  deriving (Generic, NFData)

instance Num Fan where
    fromInteger n = NAT (fromIntegral n)
    x+y           = NAT (toNat x + toNat y)
    x*y           = NAT (toNat x * toNat y)
    abs x         = x
    negate _      = NAT 0
    signum x      = case toNat x of { 0 -> NAT 0; _ -> NAT 1 }

toNat :: Fan -> Nat
toNat (NAT n) = n
toNat _       = 0

type Rex = GRex Fan

-- I want to use pattern synonyms here, but I ran into a GHC bug.

{-# INLINE mkCow #-}
mkCow :: Nat -> Fan
mkCow 0 = ROW mempty
mkCow n = COw n

{-# INLINE mkCab #-}
mkCab :: Set Fan -> Fan
mkCab ks | S.null ks = TAB M.empty
mkCab ks             = CAb ks


-- Internal DSL used to execute Laws -------------------------------------------

data Prog = PROG
    { arity :: !Int
    , stkSz :: !Int
    , prgrm :: !Run
    }

{-
   GHC Will use pointer-tagging for the first 6 constructors.
-}
data Run
    = CNS !Fan
    | ARG {-# UNPACK #-} !Int
    | VAR {-# UNPACK #-} !Int
    | LET {-# UNPACK #-} !Int !Run !Run
    | IF_ !Run !Run !Run

    | EXE                !(SmallArray Fan -> Fan)  --  Precalculated Executor
          {-# UNPACK #-} !Int                      --  Precalculated Frame Size
                         !Fan                      --  Function (or closure)
          {-# UNPACK #-} !(SmallArray Run)         --  Arguments

    | SWI      !Run !Run !(SmallArray Run)
    | JMP      !Run !Run !(Map Fan Run)
    | JMP_WORD !Run !Run !(SV.Vector Word) !(SmallArray Run)

    | SEQ !Run !Run

    | REC {-# UNPACK #-} !(SmallArray Run)      -- Saturated self-application

    | KAL {-# UNPACK #-} !(SmallArray Run)      -- Dynamic Application

    | PAR {-# UNPACK #-} !Int              -- Closure arity (avoid recalculate)
          {-# UNPACK #-} !(SmallArray Run) -- Unsaturated Application

    | TRK !Run !Run

    | MK_ROW !(Vector Run)

    | MK_TAB !(Map Fan Run)

    -- Inlined operations with fallback from the pin.
    | OP2 !String (Fan -> Fan -> Fan) !Run !Run

   -- Broken Spine (thunked sub-spine)
    | LAZ !Prog !(SmallArray Run)
