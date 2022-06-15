{-# OPTIONS_GHC -Wall   #-}
{-# OPTIONS_GHC -Werror #-}

module Sire.Types
    ( XTag(..)
    , Tag(..)
    , Cmd(..)
    , Rul(..)
    , Fun(..)
    , Bod(..)
    , Exp(..)
    , Val(..)
    , XCmd(..)
    , XLaw(..)
    , XBod(..)
    , XExp(..)
    , XFun(..)
    , XVal(..)
    , LawName(..)
    , Word256
    )
where

import PlunderPrelude

import Plun.Types (LawName(..))
import Rex        (GRex)

----------------------------------------
-- Identifier with Explicit Tag Value --
----------------------------------------

data Tag = TAG
  { tagIdn :: !Text
  , tagNam :: !LawName
  }
 deriving (Eq, Ord, Show, Generic, NFData)

------------
-- Values --
------------

data Val a
    = REF a
    | NAT Nat
    | APP (Val a) (Val a)
    | LAW LawName Nat (Bod a)
    | ROW (Vector (Val a))
    | COW Nat
    | TAB (Map Nat (Val a))
    | CAB (Set Nat)
    | BAR ByteString
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, NFData)

instance Num (Val a) where
    fromInteger = NAT . fromInteger
    (+)    = error "HACK"
    (*)    = error "HACK"
    abs    = error "HACK"
    signum = error "HACK"
    negate = error "HACK"

data XVal
    = XVREF Text
    | XVNAT Nat
    | XVAPP XVal XVal
    | XVLAW XLaw
    | XVROW (Vector XVal)
    | XVCOW Nat
    | XVTAB (Map Nat XVal)
    | XVCAB (Set Nat)
    | XVBAR ByteString
 deriving (Show, Generic, NFData)

-----------
-- Rules --
-----------

data Rul a = RUL
  { rulName :: LawName
  , rulArgs :: Nat
  , rulBody :: Bod a
  }
 deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, NFData)

data XLaw = XLAW XTag [Text] XBod
 deriving (Show, Generic, NFData)

data XBod
  = XVAR Text
  | XCNS XVal
  | XAPP XBod XBod
  | XBAD XVal
  | XLET Text XBod XBod
 deriving (Show, Generic, NFData)

---------------
-- Functions --
---------------

{-
    - `a`      -> XTAG "a" NONE       NONE
    - `a^32`   -> XTAG "a" NONE       (SOME 32)
    - `a/1`    -> XTAG "a" (SOME "1") NONE
    - `a/1^32` -> XTAG "a" (SOME "1") (SOME 32)
    - `_`      -> XTAG ""  NONE       NONE
    - `_^32`   -> XTAG ""  NONE       (SOME 32)
    - `_/1`    -> XTAG ""  (SOME "1") NONE
    - `_/1^32` -> XTAG ""  (SOME "1") (SOME 32)
-}
data XTag = XTAG !Text !(Maybe Text) !(Maybe LawName)
 deriving (Eq, Ord, Show, Generic, NFData)

data XFun z = XFUN XTag [Text] (XExp z)
 deriving (Eq, Ord, Show, Generic, NFData, Functor, Foldable, Traversable)

data XExp z
  = XEBED z
  | XEREF Text
  | XEHAZ Word256
  | XENAT Nat
  | XEBAR ByteString
  | XEAPP (XExp z) (XExp z)
  | XEREC Text (XExp z) (XExp z)
  | XELET Text (XExp z) (XExp z)
  | XEVEC [XExp z]
  | XECOW Nat
  | XECAB (Set Nat)
  | XETAB (Map Nat (XExp z))
  | XEBAT (Map Nat Text) (XExp z) (XExp z)
  | XECOR Text (XExp z) [XFun z]
  | XELAM (XFun z)
  | XELIN (XFun z)
  | XEPAT (XExp z) (XExp z) (Map Nat ([Text], XExp z))
 deriving (Eq, Ord, Show, Generic, NFData, Functor, Foldable, Traversable)

--- ?-  xs
--- ++  0,n
--- ++  1,n,t
---
--- ?+  xs
---   fallback
--- ++  0,n    |
--- ++  1,n,t  |
---
--- ?- expands to ?+
---
--- ?+ expands to `R_IDX` or `T_IDX` depending on whether keys work as
---    vector indexes or nah.
---
--- Each `[++ [, n xs..] b]` branch expands into `[* [, _ xs..] b]`
---
--- @- and @+ are atomic version of `?-` and `?+`.
---
---     @-  c
---     ++  0   %zero
---     ++  %a  %aye
---     ++  %z  %zee
---
---     @+  c
---       %other
---     ++  0   %zero
---     ++  %a  %aye
---     ++  %z  %zee
---
--- Again, just implemented as R_IDX or T_IDX.
---
--- The final boss is `|%`: Full LETREC.
---
---     |%
---     ++  [odd n]   @-  n
---                   ++  0  | false
---                   ++  _  | odd | dec n
---                   ==
---     ++  [even n]  @-  n
---                   ++  0  | true
---                   ++  _  | odd | dec n
---                   ==
---     ++  zeroOne | {0 oneZero}
---     ++  oneZero | {1 zeroOne}
---     ++  true    | oneZero
---     ++  false   | zeroOne
---     ==
---
--- Maybe we can approach that gradually, allowing values-only LETREC,
--- where the non-cyclic elements get spit off, as do the smallest cycles?

-- FUN self tag argIdns expr
data Fun z v a = FUN v LawName [v] (Exp z v a)
 deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, NFData)

type Word256 = ByteString -- Always 256 bytes

data Bod a
  = BVAR Nat
  | BCNS (Val a)
  | BAPP (Bod a) (Bod a)
  | BLET (Bod a) (Bod a)
  | BBAD (Val a)
 deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, NFData)

data Exp z v a
  = EBED z
  | EREF a
  | EHAZ Word256
  | EVAR v
  | ENAT Nat
  | EBAR ByteString
  | EAPP (Exp z v a) (Exp z v a)
  | EREC v (Exp z v a) (Exp z v a)
  | ELET v (Exp z v a) (Exp z v a)
  | EVEC [Exp z v a]
  | ECOW Nat
  | ETAB (Map Nat (Exp z v a))
  | ECAB (Set Nat)
  | EOPN [v] (Exp z v a) (Exp z v a)
  | EBAT (Map Nat v) (Exp z v a) (Exp z v a)
  | ECOR Text (Exp z v a) [(v, Fun z v a)]
  | ELAM (Fun z v a)
  | ELIN (Fun z v a)
  | EPAT (Exp z v a) (Exp z v a) (Map Nat ([Text], Exp z v a))
 deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, NFData)

-------------------
-- REPL Commands --
-------------------

data Cmd z v a
  = PRINT (Exp z v a)
  | VOPEN [a] (Exp z v a)
  | DUMPY (Exp z v a)
  | CHECK [(XExp z, Exp z v a)]
  | MKRUL a (Rul a)
  | DEFUN [(a, Fun z v a)]
  | ALIAS [(a, Val a)]
  | ANOTE [GRex z] (Maybe (GRex z))
  | SAVEV (Exp z v a)
  | IOEFF a a (Exp z v a)
  | MACRO a (Exp z v a)
  | EPLOD (XExp z)
 deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, NFData)

data XCmd z
  = XPRINT (XExp z)
  | XVOPEN [Text] (XExp z)
  | XDUMPY (XExp z)
  | XCHECK [XExp z]
  | XMKRUL XLaw
  | XALIAS [(Text, XVal)]
  | XDEFUN [XFun z]
  | XANOTE [GRex z] (Maybe (GRex z))
  | XSAVEV (XExp z)
  | XIOEFF Text Text (XExp z)
  | XMACRO Text (XExp z)
  | XPLODE (XExp z)
