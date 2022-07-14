{-# OPTIONS_GHC -Wall   #-}
{-# OPTIONS_GHC -Werror #-}

module Plun.Types
    ( Pln(..)
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
    , valTag
    )
where

import PlunderPrelude hiding (hash)

import Data.Bits     (xor)
import Data.Char     (isAlphaNum)
import Data.Hashable (hash)
import GHC.Prim      (reallyUnsafePtrEquality#)
import Data.Map (toAscList)

import qualified Data.Set as S
import qualified Data.ByteString as BS

-- Types -----------------------------------------------------------------------

{-
    Note that `lawExec` is lazy.  We don't "compile" it until it is
    first run.
-}
data Law = L
    { lawName :: !LawName
    , lawArgs :: !Nat
    , lawBody :: !Pln
    , lawExec :: [Pln] -> Pln
    }

newtype LawName = LN { lawNameNat :: Nat }
  deriving newtype (Eq, Ord, NFData, Hashable)

instance Show LawName where
    show = either show show . natUtf8 . lawNameNat

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
    , pinExec :: [Pln] -> Pln
    , pinItem :: !Pln
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
fastValEq :: Pln -> Pln -> Bool
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
    = ROW !(Vector Pln)
    | TAB !(Map Nat Pln)
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
    | APP Nod Pln
    | LAW !Law
    | DAT !Dat
    | PIN !Pin
  deriving (Eq,  Generic, NFData)

-- | A Plunder value.
data Pln = PLN
    { valArity :: !Nat
    , valNod   :: !Nod
    }
  deriving (Eq, Generic, NFData)

instance Ord Pln where
    compare (AT x) (AT y) = compare x y
    compare (PN x) (PN y) = compare (pinHash x) (pinHash y)
    compare AT{}   _      = GT
    compare _      AT{}   = LT
    compare x      y      = compare (pCar x, pCdr x) (pCar y, pCdr y)

pCar :: Pln -> Pln
pCar (PLN r n) = case n of
    NAT{}     -> AT 0
    APP x _   -> PLN (r+1) x
    LAW L{..} -> law lawName lawArgs lawBody
    DAT d     -> dataWut law (\h _ -> h) d
    PIN{}     -> AT 4
  where
    law nm ar _ = PLN 1 (NAT 0 `APP` AT (lawNameNat nm)
                               `APP` AT ar)

pCdr :: Pln -> Pln
pCdr (PLN r n) = case n of
    NAT{}      -> AT 0
    APP x _    -> PLN (r+1) x
    LAW L{..}  -> law lawName lawArgs lawBody
    DAT d      -> dataWut law (\_ t -> t) d
    PIN(P{..}) -> pinItem
  where
    law _ _  b = b

valName :: Pln -> Text
valName (PLN _ (LAW L{..})) =
    let nat = lawNameNat lawName
    in either (const $ tshow nat) id (natUtf8 nat)
valName (PLN _ (PIN(P{..}))) =
    valName pinItem
valName _ =
    "_"

valTag :: Pln -> Nat
valTag (PLN _ (LAW L{..}))  = lawNameNat lawName
valTag (PLN _ (PIN(P{..}))) = valTag pinItem
valTag _                    = 0

instance Show Nod where
    show (NAT n)   = show n
    show (APP f x) = "(" <> intercalate " " (show <$> unApp f [x]) <> ")"
    show (LAW l)   = show l
    show (PIN p)   = "[" <> unpack (valName $ pinItem p) <> "]"
    show (DAT d)   = show d

unApp :: Nod -> [Pln] -> [Pln]
unApp (APP f x) xs = unApp f (x:xs)
unApp f         xs = (nodVal f):xs

instance Show Pln where
    show (PLN _ n) = show n

instance Num Pln where
    fromInteger n = AT (fromIntegral n)
    x+y           = AT (toNat x + toNat y)
    x*y           = AT (toNat x * toNat y)
    abs x         = x
    negate _      = AT 0
    signum x      = case toNat x of { 0->AT 0; _->AT 1 }

toNat :: Pln -> Nat
toNat (PLN _ (NAT n)) = n
toNat (PLN _ _      ) = 0

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

instance Hashable Pln where
    hash (PLN _ nod) = hash nod
    hashWithSalt salt (PLN _ nod) = hashWithSalt salt nod


-- Utilities -------------------------------------------------------------------

isPin :: Pln -> Bool
isPin (PLN _ (PIN{})) = True
isPin (PLN _ _)       = False

nodVal :: Nod -> Pln
nodVal n = PLN (evalArity n) n

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
trueArity :: Pln -> Nat
trueArity = \case
    PLN _ (DAT (COW n)) -> go (DAT (COW n))
    PLN _ (DAT (CAB n)) -> go (DAT (CAB n))
    PLN _ (APP f _)     -> go f - 1
    PLN r _             -> r
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
evalArity (PIN b)   = trueArity (pinItem b)
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

pattern AT :: Nat -> Pln
pattern AT n <- PLN _ (NAT n)
  where AT n = nodVal (NAT n)

pattern PN :: Pin -> Pln
pattern PN n <- PLN _ (PIN n)
  where PN p = nodVal (PIN p)

pattern CL :: Pln -> Pln -> Pln
pattern CL x y <- PLN _ (APP (nodVal -> x) y)
  where CL x y = nodVal (APP (valNod x) y)

pattern RL :: Law -> Pln
pattern RL r <- PLN _ (LAW r)
  where RL r = nodVal (LAW r)

pattern (:&) :: Nod -> Pln -> Nod
pattern (:&) x y = APP x y

pattern (:#) :: Nod -> Pln -> Pln
pattern (:#) x y <- PLN _ (APP x y)
  where (:#) x y = nodVal (APP x y)

--------------------------------------------------------------------------------

barBody :: ByteString -> Nat
barBody bytes =
    -- TODO Make this not slow
    bytesNat (bytes <> BS.singleton 1)

-- TODO Carefully review!
dataWut
    :: ∀a
     . (LawName -> Nat -> Pln -> a)
    -> (Pln -> Pln -> a)
    -> Dat
    -> a
dataWut rul cel = \case
    ROW v -> case toList v of
                    []   -> rul (LN 0) 1 (AT 0)
                    x:xs -> apple (DAT $ COW sz) (x :| xs)
               where sz = fromIntegral (length v)
    TAB d -> tabWut d
    BAR b -> rul (LN 1) 1 (AT $ barBody b)
    COW n -> rul (LN 0) (n+1) (AT 0)
    CAB k -> cabWut k
  where
    apple :: Nod -> NonEmpty Pln -> a
    apple n (x :| [])     = cel (nodVal n) x
    apple n (v :| (x:xs)) = apple (APP n v) (x :| xs)

    tabWut :: Map Nat Pln -> a
    tabWut tab = case val of
                    []   -> cabWut mempty
                    v:vs -> apple (DAT $ CAB key) (v :| vs)
      where par = toAscList tab
            key = setFromList (fst <$> par)
            val = snd <$> par

    cabWut :: Set Nat -> a
    cabWut ks = rul (LN 0) nArgs (nodVal $ DAT $ ROW $ AT <$> k)
      where nArgs = fromIntegral (length k+1)
            k     = fromList (S.toAscList ks)
