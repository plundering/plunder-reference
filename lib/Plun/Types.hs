{-# OPTIONS_GHC -Wall   #-}
{-# OPTIONS_GHC -Werror #-}

module Plun.Types
    ( Val(..)
    , Nod(..)
    , Nat
    , Pin(..)
    , Law(..)
    , Dat(..)
    , pattern AT
    , pattern CL
    , pattern RL
    , pattern (:#)
    , pattern (:&)
    , trueArity
    , nodVal
    , lawNameText
    , fastValEq
    , natArity
    , isPin
    , LawName(..)
    , valName
    )
where

import PlunderPrelude hiding (fromList, hash)

import Data.Bits     (xor)
import Data.Char     (isAlphaNum)
import Data.Hashable (hash)
import GHC.Prim      (reallyUnsafePtrEquality#)

-- Types -----------------------------------------------------------------------

{-
    Note that `lawExec` is lazy.  We don't "compile" it until it is
    first run.
-}
data Law = L
    { lawName :: !LawName
    , lawArgs :: !Nat
    , lawBody :: !Val
    , lawExec :: [Val] -> Val
    }

newtype LawName = LN { lawNameNat :: Nat }
  deriving newtype (Eq, Ord, Show, NFData, Hashable)

instance Show Law where
    show L{..} = "{"
              <> intercalate " " [ show lawName :: String
                                 , show lawArgs :: String
                                 , show lawBody :: String
                                 ]
              <> "}"

instance Eq Law where
    (==) x@(L n a b _) y@(L nn aa bb _) =
        case reallyUnsafePtrEquality# x y of
            1# -> True
            _  -> a==aa && n==nn && b==bb

--- `pinRefs` is all the boxs that we reference, in noun-traversal order.
--- `pinBlob` is the body serialized with `jar`.
--- `pinHash` is the BLAKE3 hash of `pinBlob`.
--- TODO Use on-heap data for hash and blob (not ByteString).
data Pin = P
    { pinRefs :: Vector Pin
    , pinBlob :: ByteString
    , pinHash :: ByteString
    , pinExec :: [Val] -> Val
    , pinItem :: !Val
    }

instance Show Pin where
  show P{..} = "{" <> show pinItem <> "}"

instance Eq Pin where
    (==) x y =
        case reallyUnsafePtrEquality# x y of
            1# -> True
            _  -> pinHash x == pinHash y

-- All data already forced upon construction.
instance NFData Law where rnf = \L{} -> ()
instance NFData Pin where rnf = \P{} -> ()

{-
    Comparision of two values that are likely to be pointer-equals.

    We always do this check on laws and pins, since they are big
    structures that are usually pointer-equals if they are equals.

    We don't want to do this on every single value equality check,
    though.  It's expensive.
-}
fastValEq :: Val -> Val -> Bool
fastValEq x y =
    case reallyUnsafePtrEquality# x y of
        1# -> True
        _  -> x == y

-- TODO Need an explicit representation for BAR with a massive number
--      of extra zeros.  Otherwise, this will fail on (unreasonable)
--      code that would work perfectly fine unjetted.  For example:
--
--      ```
--      | barNat | bar bex|128 bex|128
--      ```
data Dat
    = ROW !(Vector Val)
    | TAB !(Map Nat Val)
    | BAR !ByteString
    | COW !Nat       --  Construct Row
    | CAB !(Set Nat) --  Construct Tab
  deriving (Eq, Generic, NFData)

instance Show Dat where
    show (COW n) = "R" <> show n
    show (ROW v) = "[" <> intercalate "," (show <$> v) <> "]"
    show (TAB _) = "TAB"
    show (CAB _) = "CAB"
    show (BAR b) = show b

data Nod
    = NAT !Nat
    | APP Nod Val
    | LAW !Law
    | DAT !Dat
    | PIN !Pin
  deriving (Eq,  Generic, NFData)

data Val = VAL
    { valArity :: !Nat
    , valNod   :: !Nod
    }
  deriving (Eq, Generic, NFData)

valName :: Val -> Text
valName (VAL _ (LAW L{..})) =
    let nat = lawNameNat lawName
    in either (const $ tshow nat) id (natUtf8 nat)
valName _ = "_"

instance Show Nod where
    show (NAT n)   = show n
    show (APP f x) = "(" <> intercalate " " (show <$> unApp f [x]) <> ")"
    show (LAW l)   = show l
    show (PIN p)   = "[" <> unpack (valName $ pinItem p) <> "]"
    show (DAT d)   = show d

unApp :: Nod -> [Val] -> [Val]
unApp (APP f x) xs = unApp f (x:xs)
unApp f         xs = (nodVal f):xs

instance Show Val where
    show (VAL _ n) = show n

hashMix :: Int -> Int -> Int
hashMix h1 h2 = (h1 * 16777619) `xor` h2

--- TODO `hash` should just cast to word, taking the first 64 bits.
--- TODO SHA256 should be `Bytes` instead of `ByteString`.
--- TODO Blobs should be `Bytes` instead of `ByteString`.
instance Hashable Pin where
    hash = hash . pinHash
    hashWithSalt salt x = salt `hashMix` hash x

-- TODO Optimize the shit out of this.
instance Hashable Nod where
    hash = \case
        NAT n           -> fromIntegral n
        APP x y         -> hashMix (hash x) (hash y)
        LAW (L n t _ _) -> hash n `hashMix` hash t
        DAT (BAR b)     -> hash b
        DAT (ROW r)     -> hash r
        DAT (CAB k)     -> hash k
        DAT (COW n)     -> hash n
        DAT (TAB m)     -> hash m
        PIN p           -> hash p
    hashWithSalt salt x =
        salt `hashMix` hash x

instance Hashable Val where
    hash (VAL _ nod) = hash nod
    hashWithSalt salt (VAL _ nod) = hashWithSalt salt nod


-- Utilities -------------------------------------------------------------------

isPin :: Val -> Bool
isPin (VAL _ (PIN{})) = True
isPin (VAL _ _)       = False

nodVal :: Nod -> Val
nodVal n = VAL (evalArity n) n

lawNameText :: LawName -> Text
lawNameText (LN 0) = "_"
lawNameText (LN n) =
  case natUtf8 n of
    Left _  -> fallback
    Right t ->
      let cs = unpack t
      in if | all isNameChar cs -> t
            | otherwise         -> fallback
 where
  fallback = "_/" <> tshow n

  isNameChar '_' = True
  isNameChar c   = isAlphaNum c


-- Vector and Table constructors are given an evalArity of one less than
-- there actual arity so that they will be transformed into vectors/tables
-- once saturated.
--
-- Thus, to get the true arity, we need to actually do the scan and see
-- what's at the head.
trueArity :: Val -> Nat
trueArity = \case
    VAL _ (DAT (COW n)) -> go (DAT (COW n))
    VAL _ (DAT (CAB n)) -> go (DAT (CAB n))
    VAL _ (APP f _)     -> go f - 1
    VAL r _             -> r
  where
    go = \case
        DAT (COW n) -> succ $ fromIntegral n
        DAT (CAB n) -> succ $ fromIntegral (length n)
        APP f _     -> go f - 1
        nod         -> evalArity nod

{-# INLINE natArity #-}
natArity :: Nat -> Nat
natArity 0 = 3 -- LAW
natArity 1 = 5 -- CAS
natArity 2 = 3 -- DEC
natArity _ = 1

evalArity :: Nod -> Nat
evalArity (LAW l)   = lawArgs l
evalArity (PIN b)   = valArity (pinItem b)
evalArity (NAT n)   = natArity n
evalArity (APP f _) = evalArity f - 1
evalArity (DAT d)   = djArgs d
  where
    djArgs :: Dat -> Nat
    djArgs (COW 0)          = error "Should be jet matched as V0"
    djArgs (CAB t) | null t = error "Should be jet matched as T0"
    djArgs (COW n)          = n
    djArgs (CAB n)          = fromIntegral (length n)
    djArgs (ROW _)          = 1
    djArgs (TAB _)          = 1
    djArgs (BAR _)          = 1

pattern AT :: Nat -> Val
pattern AT n <- VAL _ (NAT n)
  where AT n = nodVal (NAT n)

pattern CL :: Val -> Val -> Val
pattern CL x y <- VAL _ (APP (nodVal -> x) y)
  where CL x y = nodVal (APP (valNod x) y)

pattern RL :: Law -> Val
pattern RL r <- VAL _ (LAW r)
  where RL r = nodVal (LAW r)

pattern (:&) :: Nod -> Val -> Nod
pattern (:&) x y = APP x y

pattern (:#) :: Nod -> Val -> Val
pattern (:#) x y <- VAL _ (APP x y)
  where (:#) x y = nodVal (APP x y)
