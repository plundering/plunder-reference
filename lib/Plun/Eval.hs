{-# LANGUAGE RecursiveDo #-}
{-# OPTIONS_GHC -Wall    #-}
{-  OPTIONS_GHC -Werror  #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Plun.Eval
    ( Fan(..)
    , Nat
    , Pin(..)
    , Law(..)
    , trueArity
    , lawNameText
    , fastValEq
    , natArity
    , isPin
    , LawName(..)
    , valName
    , valTag
    , boom
    , matchData
    , toNat
    , EvalSt(..)
    , state
    , (%%)
    , (^)
    , cryptographicIdentity
    , executeLaw
    , compileLaw
    , mkPin
    , mkPin'
    , mkLaw
    , appN
    , kloList
    , kloWalk
    , mkRow
    , evalArity
    )
where

import PlunderPrelude hiding (hash, (^))
import Data.Primitive.SmallArray
import Text.Show.Pretty

import Control.Monad.ST (ST)
import Data.Bits        (xor)
import Data.Char        (isAlphaNum)
import Data.Hashable    (hash)
import Data.Vector      ((!))
import GHC.Prim         (reallyUnsafePtrEquality#)
import Jar              (capBSExn, jar)
import Jar.Nounable     (Hash256(..), NounView(..), Nounable(..))
import Jar.Util         (showJarBits)
import Plun.Print       (decodeBtc)

import qualified Prelude as Base
import qualified GHC.Exts        as GHC
import qualified BLAKE3          as B3
import qualified Data.ByteArray  as BA
import qualified Data.ByteString as BS
import qualified Data.Char       as C
import qualified Data.Map        as M
import qualified Data.Set        as S
import qualified Data.Vector     as V

-- ort Jar          (capBSExn, jar)
-- ort Jar.Nounable (Hash256(..), Nounable(..), NounView(..))


infixl 5 %%;

-- Types -----------------------------------------------------------------------

{-
    Note that `lawExec` is lazy.  We don't "compile" it until it is
    first run.
-}
data Law = L
    { lawName :: !LawName
    , lawArgs :: !Nat
    , lawBody :: !Fan
    , lawExec :: SmallArray Fan -> Fan
    }

newtype LawName = LN { lawNameNat :: Nat }
  deriving newtype (Eq, Ord, NFData, Hashable)

instance Show LawName where
    show = either show show . natUtf8 . lawNameNat

instance Show Law where
    show L{..} = concat
               [ "(LAW "
               , unpack (ugly $ lawNameNat lawName)
               , " "
               , show lawArgs
               , " "
               , show lawBody
               , ")"
               ]

instance Eq Law where
    (==) x@(L n a b _) y@(L nn aa bb _) =
        case reallyUnsafePtrEquality# x y of
            1# -> True
            _  -> a==aa && n==nn && b==bb

-- TODO What if the small array has extra shit, and that shit can't
-- safely be forced?  Don't do `mapSmallArray`, do it by hand.
normalize :: Fan -> Fan
normalize top =
    if isNormal top then top else go top
  where
    isNormal (KLO _ xs) =
        case xs^0 of
            KLO{} -> False
            _     -> all isNormal (GHC.toList xs)
    isNormal (ROW v) = all isNormal v
    isNormal (TAB t) = all isNormal t
    isNormal !_      = True

    go top = case top of
        NAT !_ -> top
        BAR !_ -> top
        PIN !_ -> top
        FUN !_ -> top
        COW !_ -> top
        CAB !_ -> top
        ROW r  -> ROW (go <$> r)
        TAB t  -> TAB (go <$> t)
        KLO r eRaw ->
            let e = mapSmallArray' go eRaw in
            case (e ^ 0) of
               KLO _ ee ->
                   let !w  = sizeofSmallArray e
                       !ww = sizeofSmallArray ee
                       len = ww + (w-1)
                   in
                       KLO r $ createSmallArray len (NAT 999) \a -> do
                                   copySmallArray a 0  ee 0 ww
                                   copySmallArray a ww e  1 (w-1)
               _ -> KLO r e

--- `pinRefs` is all the boxs that we reference, in noun-traversal order.
--- `pinBlob` is the body serialized with `jar`.
--- `pinHash` is the BLAKE3 hash of `pinBlob`.
--- TODO Use on-heap data for hash and blob (not ByteString).
data Pin = P
    { pinRefs :: Vector Pin
    , pinBlob :: ByteString
    , pinHash :: ByteString
    , pinItem :: !Fan
    , pinExec :: SmallArray Fan -> Fan
    }

instance Show Pin where
  show P{..} = unpack (valName pinItem)

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
fastValEq :: Fan -> Fan -> Bool
fastValEq x y =
    case reallyUnsafePtrEquality# x y of
        1# -> True
        _  -> x == y

{-
    I previously had the width explicitly tracked in the `KLO` node
    which was every-so-slightly faster.  Seems wasteful but saves an
    indirection in some cases, could go back.
-}
data Fan
    = NAT !Nat
    | PIN !Pin
    | FUN !Law
    | KLO !Int {-# UNPACK #-} !(SmallArray Fan)
    | BAR !ByteString
    | ROW !(Vector Fan)
    | TAB !(Map Nat Fan)
    | CAB !(Set Nat)
    | COW !Nat
  deriving (Generic, NFData)

-- TODO Make sure evaluation order is correct.  Don't evaluate more or
-- less than formal impl.
instance Eq Fan where
    NAT n   == NAT m   = (n==m)
    PIN p   == PIN q   = (p==q)
    BAR b   == BAR d   = (b==d)
    ROW r   == ROW s   = (r==s)
    TAB t   == TAB u   = (t==u)
    CAB c   == CAB d   = (c==d)
    COW n   == COW m   = (n==m)
    FUN l   == FUN a   = (l==a)
    v@KLO{} == w@KLO{} = (kloWalk v == kloWalk w)
    _       == _       = False

instance Ord Fan where
    compare (NAT x) (NAT y) = compare x y
    compare (PIN x) (PIN y) = compare (pinHash x) (pinHash y)
    compare NAT{}  _        = GT
    compare _      NAT{}    = LT
    compare x      y        = compare (boom x) (boom y)

(^) :: SmallArray a -> Int -> a
(^) = indexSmallArray

boom :: Fan -> (Fan, Fan)
boom = \case
    NAT{} ->
        (NAT 0, NAT 0)

    FUN L{..} ->
        rul lawName lawArgs lawBody

    BAR b ->
        rul (LN 1) 1 (NAT $ barBody b)

    PIN p ->
        let args = (trueArity $ pinItem p) in
        rul (LN 0) args (pinBody args $ pinItem p)

    COW n ->
        rul (LN 0) (n+1) (NAT 0)

    -- When we take the head of a closure with more than two elements,
    -- we essentially create a lazy-list of width=2 closure nodes.
    KLO arity xs ->
        case sizeofSmallArray xs of
          2   -> ( xs^0 , xs^1 )
          len -> ( let
                     flow !_ !0 = xs^0
                     flow !r !i = KLO (r+1) (a2 (flow (r+1) (i-1)) (xs^i))
                   in
                     flow arity (len-2)
                 , xs^(len-1)
                 )

    -- Builds lazy list of two-element KLO nodes.
    ROW row ->
        let !len = length row in
        case len of
            0 -> boom (COW 0)
            1 -> (COW 1, row!0)
            n -> ( let
                     flow !i !0   = COW (fromIntegral i)
                     flow !i !rem = KLO i $ a2 (flow (i+1) (rem-1)) (row!i)
                   in
                     flow 1 (n-1)
                 ,
                   row!0
                 )

    -- Builds lazy list of two-element KLO nodes.
    TAB tab ->
        let !len = length tab
            !cab = CAB (M.keysSet tab)
        in
        case len of
            0 -> boom cab
            1 -> (cab, snd (M.findMin tab))
            _ -> let
                    (fst : rest) = M.elems tab

                    fill i []     = CAB (M.keysSet tab)
                    fill i (v:vs) = KLO i (a2 (fill (i+1) vs) v)
                 in
                 ( fill 1 rest
                 , fst
                 )

    CAB ks ->
        rul (LN 0)
            (fromIntegral (length ks + 1))
            (ROW $ fromList $ fmap NAT $ S.toAscList ks)

  where
    rul :: LawName -> Nat -> Fan -> (Fan, Fan)
    rul (LN n) a b =
        ( KLO 1 (a3 (NAT 0) (NAT n) (NAT a))
        , b
        )

a2 :: a -> a -> SmallArray a
a2 p q = createSmallArray 2 p \a -> do
    writeSmallArray a 1 q

a3 :: a -> a -> a -> SmallArray a
a3 p q r = createSmallArray 3 p \a -> do
    writeSmallArray a 1 q
    writeSmallArray a 2 r

a4 :: a -> a -> a -> a -> SmallArray a
a4 p q r s = createSmallArray 4 p \a -> do
    writeSmallArray a 1 q
    writeSmallArray a 2 r
    writeSmallArray a 3 s

valName :: Fan -> Text
valName (FUN L{..}) = ugul (lawNameNat lawName)
valName (PIN P{..}) = valName pinItem
valName _           = "_"

valTag :: Fan -> Nat
valTag (FUN L{..}) = lawNameNat lawName
valTag (PIN P{..}) = valTag pinItem
valTag _           = 0

kloList :: Fan -> [Fan]
kloList = reverse . kloWalk

kloWalk :: Fan -> [Fan]
kloWalk (KLO _ xs) = go (sizeofSmallArray xs - 1)
                       where
                         go !0 = kloWalk (xs^0)
                         go !i = (xs^i) : go (i-1)
kloWalk v          = [v]

instance Show Fan where
    show (NAT n)   = ugly n
    show (KLO _ x) = show $ GHC.toList x
    show (FUN l)   = show l
    show (PIN p)   = show p
    show (COW n)   = "R" <> show n
    show (ROW v)   = "(ROW " <> show v <> ")"
    show (TAB t)   = "(TAB " <> show (showTab t) <> ")"
    show (CAB k)   = "(CAB " <> show (toList k) <> ")"
    show (BAR b)   = "(BAR " <> show b <> ")"

showTab :: Map Nat Fan -> [(Fan,Fan)]
showTab t = M.toList t <&> \(k, v) -> (NAT k, v)

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

hashMix :: Int -> Int -> Int
hashMix h1 h2 = (h1 * 16777619) `xor` h2

--- TODO `hash` should just cast to word, taking the first 64 bits.
--- TODO SHA256 should be `Bytes` instead of `ByteString`.
--- TODO Blobs should be `Bytes` instead of `ByteString`.
instance Hashable Pin where
    hash = hash . pinHash
    hashWithSalt salt x = salt `hashMix` hash x

-- TODO Optimize the shit out of this.
instance Hashable Fan where
    hash = \case
        NAT n           -> fromIntegral n
        v@KLO{}         -> hashMix (hash h) (hash t) where (h,t) = boom v
        FUN (L n t _ _) -> hash n `hashMix` hash t
        PIN p           -> hash p
        BAR b           -> hash b
        ROW r           -> hash r
        CAB k           -> hash k
        COW n           -> hash n
        TAB m           -> hash m
    hashWithSalt salt x =
        salt `hashMix` hash x


-- Utilities -------------------------------------------------------------------

isPin :: Fan -> Bool
isPin PIN{} = True
isPin _     = False

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
trueArity :: Fan -> Nat
trueArity = \case
    COW 0          -> error "Should be jet matched as V0"
    COW n          -> succ $ fromIntegral n
    CAB t | null t -> error "Should be jet matched as T0"
    CAB n          -> succ $ fromIntegral (length n)
    KLO _ xs       -> trueArity (xs^0) - fromIntegral (sizeofSmallArray xs - 1)
    FUN l          -> lawArgs l
    NAT n          -> natArity n
    PIN b          -> trueArity (pinItem b)
    ROW _          -> 1
    TAB _          -> 1
    BAR _          -> 1

{-# INLINE natArity #-}
natArity :: Nat -> Nat
natArity 0 = 3 -- FUN
natArity 1 = 4 -- CAS
natArity 2 = 3 -- DEC
natArity _ = 1

evalArity :: Fan -> Int
evalArity (FUN l)          = natToArity (lawArgs l)
evalArity (NAT 0)          = 3
evalArity (NAT 1)          = 4
evalArity (NAT 2)          = 3
evalArity (NAT _)          = 1
evalArity (KLO r _)        = r
evalArity (PIN b)          = natToArity $ trueArity (pinItem b)
evalArity (COW 0)          = error "Should be jet matched as V0"
evalArity (CAB t) | null t = error "Should be jet matched as T0"
evalArity (COW n)          = natToArity n
evalArity (CAB n)          = length n
evalArity (ROW _)          = 1
evalArity (TAB _)          = 1
evalArity (BAR _)          = 1

--------------------------------------------------------------------------------

barBody :: ByteString -> Nat
barBody bytes =
    -- TODO Make this not slow
    bytesNat (bytes <> BS.singleton 1)

pinBody :: Nat -> Fan -> Fan
pinBody 0 x = KLO 1 $ a2 (NAT 2) x
pinBody n x = KLO 1 $ a3 (NAT 0) (pinBody(n-1) x) (NAT n)

--------------------------------------------------------------------------------

matchData :: LawName -> Nat -> Fan -> Maybe Fan
matchData (LN 0) 1 (NAT 0)     = Just $ ROW (fromList [])
matchData (LN 0) n (NAT 0)     = Just $ COW (n-1)
matchData (LN 0) n (ROW v)     = matchCab n v
matchData (LN 0) n x@KLO{}     = mkPin <$> matchPin n x
matchData (LN 1) 1 (NAT n)     = matchBar n
matchData (LN _) _ _           = Nothing

matchBar :: Nat -> Maybe Fan
matchBar n = do
    guard (n /= 0)
    let bitWidth = (natBitWidth n :: Nat) - 1
    guard (0 == (bitWidth `mod` 8))
    let bytWidth = fromIntegral (bitWidth `div` 8)
    pure $ BAR $ take bytWidth $ natBytes n

-- Input is normalized, so head of KLO is never KLO, and closure
-- environment array always matches closure size.
matchPin :: Nat -> Fan -> Maybe Fan
matchPin n (KLO _ e) =
    case (n, GHC.toList e) of
        (0, [NAT 2, x])               -> Just x
        (0, _)                        -> Nothing
        (n, [NAT 0, x, NAT m]) | n==m -> matchPin (n-1) x
        (_, _)                        -> Nothing
matchPin _ _ =
    Nothing

matchCab :: Nat -> Vector Fan -> Maybe Fan
matchCab arity vs = do
    guard (arity == (1 + fromIntegral (length vs)))
    case toList vs of
        []         -> TAB <$> pure mempty
        NAT a : es -> CAB <$> collect mempty a es
        _          -> Nothing
  where
    collect !acc i []                 = pure (insertSet i acc)
    collect !acc i (NAT w : ws) | w>i = collect (insertSet i acc) w ws
    collect _    _ _                  = Nothing


-- Law Book State --------------------------------------------------------------

{-
    - `stBook` is a table of all rules, for deduplication.
    - `stFast` is a configurable optimizer.
-}
data EvalSt = EVAL_ST
    { stBook :: Map ByteString Pin
    , stFast :: Pin -> Pin
    }

state :: IORef EvalSt
state = unsafePerformIO $ newIORef $ EVAL_ST{..}
  where
    stBook = mempty
    stFast = \x -> x

loadPin :: MonadIO m => ByteString -> m (Maybe Pin)
loadPin hazh = liftIO $ do
    EVAL_ST{..} <- readIORef state
    pure $ lookup hazh stBook

-- Constructing Pins and Laws --------------------------------------------------

overBook :: (Map ByteString Pin -> Map ByteString Pin) -> EvalSt -> EvalSt
overBook f s = s { stBook = f (stBook s) }

mkLaw :: LawName -> Nat -> Fan -> Fan
mkLaw nam arg (normalize -> bod) = sel
  where
    prg = compileLaw nam arg bod
    law = FUN $ L nam arg bod (executeLaw sel prg)
    sel =
        if arg==0
          then
            let res = executeLaw res prg
                    $ createSmallArray 1 res \_ -> do
                          pure ()
            in res
          else
            fromMaybe law $ matchData nam arg bod

{-
    -   Jam body prefixed by reference-list.
    -   The reference list is terminated by the 0 hash.
-}
cryptographicIdentity :: Vector Pin -> ByteString -> ByteString
cryptographicIdentity refs jamBody =
    digestBytes
        $ B3.hash
        $ toList (fmap pinHash refs) <> [replicate 32 0, jamBody]
  where
    digestBytes :: B3.Digest 32 -> ByteString
    digestBytes d = BA.copyAndFreeze d (const $ pure ())

mkPin :: Fan -> Fan
mkPin = PIN . unsafePerformIO . mkPin'

frameSize :: Fan -> Int
frameSize (KLO _ e) = frameSize (e^0)
frameSize v         = 1 + evalArity v

mkPin' :: MonadIO m => Fan -> m Pin
mkPin' inp = do
    -- let putNam tx = putStrLn ("\n\n==== [[[" <> tx <> "]]] ====\n")
    -- putNam (valName inp)

    core        <- evaluate (normalize inp)
    EVAL_ST{..} <- readIORef state
    (ref, byt)  <- jar core

    -- print ("JAR_BYTES" :: Text, length byt)
    -- showJarBits core

    -- Just for debugging purposes, we deserialize each pin and validate.
    capBSExn (PIN <$> ref) byt >>= \case
        trip | trip==core -> pure ()
        trip              -> do putStrLn ("INP:" <> tshow core)
                                showJarBits core
                                putStrLn ("OUT:" <> tshow trip)
                                showJarBits trip
                                error "sour jam"

    let haz = cryptographicIdentity ref byt
        getSrc (PIN p) = getSrc (pinItem p)
        getSrc x       = x
        !src = getSrc core
        !stk = frameSize src
        !exe = valCode src
        res = stFast (P ref byt haz core exe)

    -- TODO loadPin breaks lazy evaluation of the hash.
    -- Some way to avoid this?
    --
    -- Specifically, if we eagerly do global deduplication, then we
    -- must eagerly serialize and eagerly hash.  This is probably
    -- not too much of a problem, but it also isn't great.
    --
    -- For jets, we only need to check the hash if the tag matches.
    --
    -- Really, we want global deduplication on disk, and *eventual*
    -- deduplication in memory.
    --
    -- One possible solution is to have the hash be an
    -- `unsafePerformIO` thunk that first calculates the hash,
    -- and then deduplicates through mutation.
    --
    -- This would require making `pinItem` an IORef.  The performance
    -- impact of that would be relatively small.
    --
    -- However, pinExec would still contain a reference to `lawExec`,
    -- which would contain it's own copy of the compiled code and
    -- that code would contain references to all constant values
    -- referenced in the code.  That's potentially of significant size.
    --
    -- Making `pinExec` be an IORef probably *would* have a
    -- significant effect on performance.  That's an extra indirection
    -- and a dereference for every single function call.
    --
    -- Might be best to just accept the eager deduplication in the
    -- Haskell impl.  Will also make snapshotting faster.  Just write
    -- to disk what has already been computed.
    loadPin haz >>= \case
       Just loaded -> pure loaded
       Nothing     -> atomicModifyIORef' state \st ->
                        (,) (overBook (insertMap haz res) st)
                            res


-- Evaluation ------------------------------------------------------------------


natToArity :: Nat -> Int
natToArity !n =
    if n>fromIntegral(maxBound::Int)
    then maxBound
    else fromIntegral n

(%%) :: Fan -> Fan -> Fan
(%%) = app2

data APPLY = APPLY (Int, Int) [Fan]
 deriving (Show)

app2 :: Fan -> Fan -> Fan
app2 f x =
    case evalArity f of
        1    -> eval2 f x
        args -> KLO (args-1) (a2 f x)

app3 :: Fan -> Fan -> Fan -> Fan
app3 f x y =
    case evalArity f of
        1    -> app2 (eval2 f x) y
        2    -> eval3 f x y
        args -> KLO (args-2) (a3 f x y)

app4 :: Fan -> Fan -> Fan -> Fan -> Fan
app4 f x y z =
    case evalArity f of
        1    -> app3 (eval2 f x) y z
        2    -> app2 (eval3 f x y) z
        3    -> eval4 f x y z
        args -> KLO (args-3) (a4 f x y z)

appN :: SmallArray Fan -> Fan
appN xs =
    case sizeofSmallArray xs of
       2 -> app2 (xs^0) (xs^1)
       3 -> app3 (xs^0) (xs^1) (xs^2)
       4 -> app4 (xs^0) (xs^1) (xs^2) (xs^3)
       !wid ->
            let !arity = evalArity (xs^0)
                !need  = arity+1
            in
            -- trace (ppShow $ APPLY (wid,need) (GHC.toList xs))
            case compare wid need of
                EQ -> evalN (KLO 0 xs)
                LT -> KLO (need-wid) xs
                GT -> let
                          !hed = evalN $ KLO 0 (cloneSmallArray xs 0 need)
                          !xtr = wid - need
                      in
                          appN $ createSmallArray (xtr+1) hed \buf -> do
                                     copySmallArray buf 1 xs need xtr

cow :: Nat -> Fan
cow 0 = ROW mempty
cow n = COW n

execFrame :: SmallArray Fan -> Fan
execFrame buf =
    -- hak <- freezeSmallArray buf 0 (sizeofSmallMutableArray buf)
    -- let frm = frameSize (buf^0)
    -- traceM $ ppShow $ EXEC frm (GHC.toList hak)

    -- trace ("EXEC_FRAME: " <> show (GHC.toList buf))

    case buf^0 of
        FUN f -> lawExec f buf
        PIN p -> pinExec p buf
        ROW v -> cow (fromIntegral $ length v)
        KLO{} -> error "Invalid stack frame, closure as head"
        NAT n -> execNat n buf
        BAR b -> if null b then buf^1 else NAT (barBody b)
        TAB t -> ROW $ fromList $ M.elems t
        COW n ->
            let !las = fromIntegral n in
            ROW $ V.generate (fromIntegral n) \i ->
                      (buf ^ (las - i))


        CAB ks -> TAB $ M.fromDistinctAscList (zip keyList valList)
          where
            getV 0 = []
            getV n = (buf^n) : getV (n-1)

            valList = getV (length ks)
            keyList = S.toAscList ks

eval2 :: Fan -> Fan -> Fan
eval2 fun arg =
    case fun of
        k@(KLO _ x) ->
            case x^0 of
               KLO{} -> evalN (KLO 0 $ a2 k arg)
               fun   -> let !w = sizeofSmallArray x in
                        valCode fun $ createSmallArray (w+1) arg \buf -> do
                                          copySmallArray buf 0 x 0 w
        _ ->
            valCode fun (a2 fun arg)

eval3 :: Fan -> Fan -> Fan -> Fan
eval3 fun a1 a2 =
    case fun of
        k@(KLO _ x) ->
            case x^0 of
               KLO{} -> evalN (KLO 0 $ a3 k a1 a2)
               fun   -> let !w = sizeofSmallArray x in
                        valCode fun $ createSmallArray (w+2) a2 \buf -> do
                                          copySmallArray buf 0 x 0 w
                                          writeSmallArray buf w     a1
        _ ->
            valCode fun (a3 fun a1 a2)

eval4 :: Fan -> Fan -> Fan -> Fan -> Fan
eval4 fun a1 a2 a3 =
    case fun of
        k@(KLO _ x) ->
            case x^0 of
               KLO{} -> evalN (KLO 0 $ a4 k a1 a2 a3)
               fun   -> let !w = sizeofSmallArray x in
                        valCode fun $ createSmallArray (w+3) a3 \buf -> do
                                          copySmallArray buf 0 x 0 w
                                          writeSmallArray buf w     a1
                                          writeSmallArray buf (w+1) a2
        _ ->
            valCode fun (a4 fun a1 a2 a3)



-- For example, to eval (f x y) do `evalN (KLO 0 3 [f,x,y])`.
evalN :: Fan -> Fan
evalN env =
    -- trace ("evalN: " <> show env)
    -- trace ("evalN: " <> show (frameSize env))
    execFrame $ createSmallArray (frameSize env) (NAT 0) \a -> do
                    void (fill a env)
  where
    fill :: ∀s. SmallMutableArray s Fan -> Fan -> ST s Int
    fill buf = \case
        KLO _ e -> do
            !i <- fill buf (e^0)
            let !w = sizeofSmallArray e
            let !v = w-1
            copySmallArray buf i e 1 v
            pure (i+v)
        hed ->
            writeSmallArray buf 0 hed $> 1

execNat :: Nat -> SmallArray Fan -> Fan
execNat 0 e = mkLaw (LN $ toNat $ e^1) (toNat $ e^2) (e^3)
execNat 1 e = wut (e^1) (e^2) (e^3) (e^4)
execNat 2 e = case toNat (e^3) of
                  0 -> e^1
                  n -> (e^2) %% NAT(n-1)
execNat 3 e = NAT (toNat(e^1) + 1)
execNat _ _ = NAT 0

wut :: Fan -> Fan -> Fan -> Fan -> Fan
wut f a n = \case
    x@NAT{} -> n %% x
    x@KLO{} -> let (h,t) = boom x in app3 a h t
    FUN L{..} ->
        let nm = NAT (lawNameNat lawName)
            ar = NAT lawArgs
        in app4 f nm ar lawBody
    PIN p ->
        let ar = trueArity (pinItem p)
            bo = pinBody ar $ pinItem p
        in app4 f (NAT 0) (NAT ar) bo

    BAR b -> app4 f (NAT 1) (NAT 1) (NAT $ barBody b)
    COW m -> app4 f (NAT 0) (NAT (m+1)) (NAT 0)

    v@(TAB t) ->
        if null t then wut f a n (CAB mempty) else
        let (h,t) = boom v
        in app3 a h t

    CAB k ->
        let args = NAT $ fromIntegral(length k) + 1
            keyz = ROW $ fromList $ fmap NAT $ S.toAscList k
        in app4 f (NAT 0) args keyz

    x@(ROW v) ->
        if null v
        then (wut f a n $ COW 0)
        else app3 a h t where (h,t) = boom x


{-
    PIN p -> rul (LN 0) args (pinBody args $ pinItem p)
               where args = (trueArity $ pinItem p)
    ROW v -> case reverse (toList v) of
                    []   -> rul (LN 0) 1 (AT 0)
                    x:xs -> apple (DAT $ COW sz) (x :| xs)
               where sz = fromIntegral (length v)
    TAB d -> tabWut d
    BAR b -> rul (LN 1) 1 (AT $ barBody b)
    COW n -> rul (LN 0) (n+1) (AT 0)
    CAB k -> cabWut k
-}

{-
    -- DAT dj    -> dataWut goLaw goApp dj
      -- where
        -- goApp g y      = a %% g %% y
        -- goLaw nm ar bd = f %% NAT (lawNameNat nm) %% NAT ar %% bd

dataWut
    :: ∀a
     . (LawName -> Nat -> Pln -> a)
    -> (Pln -> Pln -> a)
    -> Dat
    -> a
dataWut rul cel = \case
-}



data Prog = PROG
    { arity :: !Int
    , stkSz :: !Int
    , prgrm :: !Run
    }
  deriving (Show)

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

    | SEQ !Run !Run

    | REC {-# UNPACK #-} !(SmallArray Run)      -- Saturated self-application

    | KAL {-# UNPACK #-} !(SmallArray Run)      -- Dynamic Application

    | PAR {-# UNPACK #-} !Int
          {-# UNPACK #-} !(SmallArray Run) -- Unsaturated Application

    | TRK !Text !Run

cnsName :: Fan -> String
cnsName v =
  let res = valName v
  in if (null res || any (not . C.isPrint) res)
     then show v
     else unpack res

showCns :: Fan -> String
showCns v@KLO{}  = "{" <> intercalate " " (showCns <$> kloList v) <> "}"
showCns v@FUN{}  = cnsName v
showCns v@PIN{}  = cnsName v
showCns (ROW xs) = "(row " <> intercalate " " (fmap showCns xs) <> ")"
showCns COW{}    = "COW"
showCns TAB{}    = "TAB"
showCns CAB{}    = "CAB"
showCns BAR{}    = "BAR"
showCns (NAT n)  = show n

instance Show Run where
    show (CNS c) = showCns c
    show (ARG i) = "$" <> show i
    show (VAR i) = "#" <> show i
    show (KAL xs) = "?(" <> intercalate " " (show <$> GHC.toList xs) <> ")"

    show (EXE _ _ f xs) =
        "(" <> showCns f <> " " <> intercalate " " (show <$> GHC.toList xs) <> ")"

    show (PAR n xs) =
        "%" <> show n <> "(" <> intercalate " " (show <$> GHC.toList xs) <> ")"

    show (REC xs) = "(REC " <> intercalate " " (show <$> GHC.toList xs) <> ")"

    show (TRK _ b) = show b -- TODO

    show (LET i x v) =
        "(#let " <> show (VAR i) <> " " <> show x <> " " <> show v <> ")"

    show (IF_ c t e) =
        "(#if " <> show c <> " " <> show t <> " " <> show e <> ")"

    show (SEQ x b) =
        "(#seq " <> show x <> " " <> show b <> ")"

{-
    -   TODO Optimize SWITCH like IF_.
    -   TODO Optimize TAB_SWITCH.

    -   TODO Optimize tail recursion (no call overhead at all, just
        overwrite the arguments and re-enter the code).

    -   TODO: Calling convention should just be `SmallArray Fan`

        we should execute directly against that.  If there are let
        bindings, then lawExec should allocate it's own buffer for them
        (separate REF into VAR and ARG)

        This means that all functions without LET bindings (including
        jets) just execute directly against the closure environment.

        A thunk is then represented in the runtime system as `(exec env)`.

        We don't have to care about overflow when converting to `Int`.
        If the arity is that big, this will never be run.
-}
optimize :: Run -> Run
optimize = go
  where
    ifHash    = decodeBtc "3by2HMZG752GK3emojoLEdeJDbemCWnPC7tVbjQwhCrF"
    ifNotHash = decodeBtc "9ZTWHAx7G5YKyq4HNa5anatbE2tySSj6SCyyFHd3VDGo"
    seqHash   = decodeBtc "4QHDnmWXTpSxkGvRSWpZGcB7x2dCWh6AvyWCBmACEYnV"
    go = \case
        exe@(EXE _ _ (PIN p) args) ->
            if | pinHash p == ifHash    -> IF_ (go $ args^0) (go $ args^1) (go $ args^2)
               | pinHash p == ifNotHash -> IF_ (go $ args^0) (go $ args^2) (go $ args^1)
               | pinHash p == seqHash   -> SEQ (go $ args^0) (go $ args^1)
               | otherwise              -> exe
        LET i v b                                -> LET i v (go b)
        exe                                      -> exe

valCode :: Fan -> (SmallArray Fan -> Fan)
valCode = \case
    KLO _ x -> valCode (x^0)
    FUN f   -> lawExec f
    PIN p   -> pinExec p
    NAT n   -> execNat n
    ROW{}   -> execFrame
    BAR{}   -> execFrame
    TAB{}   -> execFrame
    COW{}   -> execFrame
    CAB{}   -> execFrame

optizolve :: Int -> Run -> Run
optizolve selfArgs = go
  where
    go EXE{}       = error "impossible"
    go PAR{}       = error "impossible"

    go c@CNS{}     = c
    go r@VAR{}     = r
    go a@ARG{}     = a

    -- go (EXE f xs)  = EXE f xs
    -- go (PAR i xs)  = PAR i xs
    go (LET i v b) = LET i (go v) (go b)
    go (KAL xs)    = kal (GHC.toList xs)

    kal (KAL ks : xs) = kal (GHC.toList ks <> xs)
    kal (CNS c  : xs) = cns c (go <$> xs)
    kal (ARG 0  : xs) = sel (go <$> xs)
    kal xs            = KAL (smallArrayFromList $ go <$> xs)

    cns :: Fan -> [Run] -> Run
    cns f xs =
        let len = fromIntegral (length xs)
            r   = evalArity f
        in
        case compare r len of
          -- TODO work harder to keep these flat?
          GT -> PAR (r-len) (smallArrayFromList (CNS f : xs))
          EQ -> EXE (valCode f) (frameSize f) f (smallArrayFromList xs)
          LT -> KAL $ smallArrayFromList
                    $ (EXE (valCode f) (frameSize f) f (smallArrayFromList $ take r xs) : drop r xs)

    sel :: [Run] -> Run
    sel xs =
        let len = fromIntegral (length xs)
            r   = selfArgs
        in
        case compare r len of
          -- TODO work harder to keep these flat?
          GT -> PAR (r-len) (smallArrayFromList (ARG 0 : xs))
          EQ -> REC $ smallArrayFromList xs
          LT -> KAL $ smallArrayFromList
                    $ ((REC $ smallArrayFromList $ take r xs) : drop r xs)

ugg :: LawName -> Text
ugg (LN n)   = ug n

ug :: Nat -> Text
ug 0   = "anon"
ug nat =
    let ok '_' = True
        ok c   = C.isAlphaNum c
    in
    case natUtf8 nat of
        Right t | all ok t -> t
        _                  -> "$" <> (tshow nat)

ugul :: Nat -> Text
ugul 0   = "anon"
ugul nat =
    let ok '_' = True
        ok c   = C.isAlphaNum c
    in
    case natUtf8 nat of
        Right t | all ok t -> t
        _                  -> tshow nat

ugly :: Nat -> String
ugly 0 = "0"
ugly nat =
    let ok '_' = True
        ok c   = C.isAlphaNum c
    in
    case natUtf8 nat of
        Right t | all ok t -> show t
        _                  -> show nat

compileLaw :: LawName -> Nat -> Fan -> Prog
compileLaw lNam numArgs lBod =
    let (n,a,b) = (lNam, numArgs, lBod)
        (maxRef, code) = go a b
        run = optimize (optizolve (natToArity a) code)
    in
       -- trace ( unpack (ugg n) <> "  :=  " <> show code <> "\n\n" <>
       --        unpack (ugg n) <> " ::== " <> show run <> "\n"
       --      )
       PROG (fromIntegral a)
            (fromIntegral maxRef + 1)
            run
  where
    go :: Nat -> Fan -> (Nat, Run)
    go maxRef fan =
        case (kloList fan) of
            [NAT n] | n<=maxRef ->
                if n<=numArgs
                then (,) maxRef (ARG $ fromIntegral n)
                else (,) maxRef (VAR $ fromIntegral (n-(numArgs+1)))

            [NAT 0, f, x] ->
                    (,) (max fMax xMax) (KAL $ a2 fRun xRun)
                  where (fMax, fRun) = go maxRef f
                        (xMax, xRun) = go maxRef x

            [NAT 1, v, b] -> (,) (max vMax bMax) (LET idx vRun bRun)
                                  where (vMax, vRun) = go (maxRef+1) v
                                        (bMax, bRun) = go (maxRef+1) b
                                        idx          = fromIntegral ((maxRef+1)-(numArgs+1))
            [NAT 2, x]    -> (maxRef, CNS x)
            _             -> (maxRef, CNS fan)

-- TODO Can implement a pure version of this for functions without LET.
-- It can be totally lazy.
executeLaw :: Fan -> Prog -> SmallArray Fan -> Fan
executeLaw self pro@PROG{prgrm,stkSz} args =
    unsafePerformIO do
        let numArgs = Base.length args
        let numVars = stkSz - numArgs
        -- traceM ("EXECUTING: " <> show pro <> "\n" <>
                -- "AGAINST: " <> show(GHC.toList args)
               -- )

        -- traceM ("EXECUTE LAW: " <> show self)
        -- traceM ("\t" <> show self)
        -- traceM ("\t" <> show pro)
        -- traceM ("\t" <> show (numArgs, numVars))
        -- traceM ("\t" <> show (GHC.toList args))
        vs <- newSmallArray numVars (error ("UNINITIALIZED" <> show prgrm))
        go vs prgrm
  where
    envSize = sizeofSmallArray args

    go vs = \case
        CNS v  -> pure v
        ARG 0  -> pure self
        ARG i  -> indexSmallArrayM args i
        VAR i  -> readSmallArray vs i
        KAL xs -> do
            -- traceM "KAL"
            cs <- traverse (go vs) xs
            pure (appN cs)

        LET i v b -> mdo
            -- traceM "LET"
            vRes <- (writeSmallArray vs i vRes >> go vs v)
            go vs b

        PAR r xs -> do
            -- traceM "PAR"
            env <- Base.traverse (go vs) xs
            pure (KLO r env)

        EXE x sz (KLO _ e) xs -> do
            -- traceM "EXE_KLO"
            !buf <- newSmallArray sz (error "dur")
            let w = sizeofSmallArray e
            copySmallArray buf 0 e 0 w
            let !nar = sizeofSmallArray xs
            let fill i = unless (i==nar) do
                             v <- go vs (xs^i)
                             writeSmallArray buf (i+w) v
                             fill (i+1)
            fill 0
            env <- unsafeFreezeSmallArray buf
            pure (x env)

        EXE x sz f xs -> do
            !buf <- newSmallArray sz f
            let !nar = sizeofSmallArray xs
            let fill i = unless (i==nar) do
                             v <- go vs (xs^i)
                             writeSmallArray buf (i+1) v
                             fill (i+1)
            fill 0
            env <- unsafeFreezeSmallArray buf
            pure (x env)

        REC xs -> do
            !buf <- newSmallArray envSize self
            let !nxs = (envSize-1)
                fill i = unless (i==nxs) do
                             v <- go vs (xs^i)
                             writeSmallArray buf (i+1) v
                             fill (i+1)
            fill 0
            env <- unsafeFreezeSmallArray buf
            pure (executeLaw self pro env)

        SEQ x b -> do
            -- traceM "SEQ"
            xv <- go vs x
            _ <- evaluate xv
            go vs b

        IF_ i t e -> do
            -- traceM "IF_"
            go vs i >>= \case
                NAT 0 -> go vs e
                NAT _ -> go vs t
                _     -> go vs e


-- WHAT EVEN -------------------------------------------------------------------

mkRow :: [Fan] -> Fan
mkRow = ROW . fromList


-- Jar for Vals ----------------------------------------------------------------

instance Nounable Fan where
    type NounRef Fan = Pin
    mkCell = (%%)
    mkAtom = NAT
    mkRefr = PIN

    nounView = \case
        NAT n -> NATISH n
        PIN p -> REFISH p (Hash256 $ pinHash p)
        v     -> uncurry APPISH (boom v)
