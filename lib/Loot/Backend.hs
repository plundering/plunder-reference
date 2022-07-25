{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Werror #-}

{- |
We want to produce a mapping from names to rules.  We'd *like* to
just assign to each rule it's tag decoded as Utf8.  And indeed, in the
common case we would do just that.  But the edge cases must be handled:

-   Tags can be any nat, most of which are not valid identifiers.
-   Multiple rules may share the same tag.
-   Some rules with non-unique tags are best shown with specific names.
    In particular, it would be nice to show a three-item vectors as
   `(V3 x y z)`

Here are the rules we use to resolve this problem:

-   First, we map every tag to an initial name:

    -   If a rule is a vector constructor (Having the noun `(1 (4+1)
        "ROW" 0)`), then we assign the name `V4`.

    -   If a rule is a a valid record constructor (Having the noun-form
        `(1 (4+1) "ROW" {"a" "b" "c" "d"})` where the keys are valid
        identifiers, then we assign the initial name `V4-a-b-c-d`

-   Second, we expect all tags to be valid identifiers [IDN]. (Note that
    rule identifiers may not contain `_`)

    Anything that doesn't follow these rules is simply assigned the
    (pre-disambiguation) name "".

-   We disambiguate by simply assigning a numeric suffix to each
    ambiguous name.  We do this in "environment order" so that we can
    achieve deterministic output:  `x_1` `x_2` `x_3` `x-y_1` `x-y_2`.

-   After this disambiguation process, we replace the empty name with "_".

-   At this point, all rules have a unique name.

    Since the initial names do not contain `_`, we don't have to worry
    about our disambiguation process producing a name that is already taken.

[IDN]:

    -   A valid "basic identifier" is a non-empty sequence of alpha
        numeric characters.  (Note that `_` is not accepted here).

    -   Identifiers are a non-empty sequence of basic identifiers
        separated by the `-`.

    -   Identifiers may not begin with a digit.

    Accepted: `{a a-b a0 a-0}`

    Not Accepted: `{_ a_ 5 0a a- -foo -foo-bar}`
-}

module Loot.Backend
    ( Closure(..)
    , NamedClosure(..)
    , nameClosure
    , rulePlunRaw
    , rulePlunOpt
    , valBod
    , bodVal
    , optimizeRul
    , Pln
    , plunSave
    , plunLoad
    , valPlun
    , plunVal
    , loadClosure
    , loadShallow
    , isPlnCodeShaped
    , isValCodeShaped
    )
where

import PlunderPrelude
import Loot.Types
import System.Directory
import Plun.Print

import Control.Monad.State  (State, evalState, runState, get, put)
import Data.Vector          ((!))
import Jar                  (capBSExn)
import Loot                 (keyRex)
import Plun                 (pattern AT, (%%), Pln(PLN))
import Plun.Types           (pattern (:#), pattern (:&), Pin(..))
import Rex.Print            (RexColorScheme(NoColors), rexLine)

-- ort qualified Data.Char   as C
-- ort qualified Data.Text   as T
import qualified Data.Vector as V
import qualified Plun      as P

--------------------------------------------------------------------------------

--  Every pin in `closureEnv` must come before all references to that pin.
--  Printing the pins and reloading them one-by-one should produce the
--  same noun.
--
--  Furthermore, in order to get deterministic output.  We expect the rule
--  order to be defined by a depth-first, head-first traversal of the
--  entire noun.
--
--  Shallow loads can be done by loading some of them pins, and loading
--  only the names of the pins loading the names of the unloaded pins
--  that they reference.
--
--  The "name" of a pin is the name of the rule in it, or just 0.
data Closure = CLOSURE
    { closureEnv :: Vector (P.Pin, Maybe (Val Int))
    , closureVal :: Val Int
    }
  deriving (Show, Generic, NFData)

data NamedClosure = NAMED_CLOSURE
    { nmz :: Vector Symb
    , env :: Map Symb (Val Symb, ByteString)
    , val :: Val Symb
    }
  deriving (Show, Generic, NFData)

rulePlunRaw :: Rul Pln -> Pln
rulePlunRaw = valPlun . rulVal

rulePlunOpt :: Rul Pln -> Pln
rulePlunOpt = valPlun . rulVal . optimizeRul

rulVal :: Rul a -> Val a
rulVal (RUL (LN _)  0  bd) = zeroArgRule bd
rulVal (RUL (LN nm) ar bd) =
    NAT 0 `APP` (NAT $ fromIntegral nm)
          `APP` (NAT ar)
          `APP` (bodVal bd)

{-
    Zero arity rule is fudged into a one arity rule pre-applied to `0`.
    All variable references are bumped and self-references return `NAT-0`.

    This is basically a hack to make the `[foo]:=3` syntax work, which
    should probably be handled in the parser instead.
-}
zeroArgRule :: Bod a -> Val a
zeroArgRule bd =
    APP (rulVal $ RUL (LN 0) 1 $ go bd) (NAT 0)
  where
    go (BCNS v)   = BCNS v
    go (BBAD v)   = BBAD v
    go (BVAR 0)   = BCNS (NAT 0)
    go (BVAR n)   = BVAR (n+1)
    go (BAPP x y) = BAPP (go x) (go y)
    go (BLET v b) = BLET (go v) (go b)


{-
    Removes extraneous CNS
-}
optimizeRul :: Rul Pln -> Rul Pln
optimizeRul (RUL nm ar bd) =
    RUL nm ar (go ar bd)
  where
    go :: Nat -> Bod Pln -> Bod Pln
    go maxArg = \case
        BLET v b -> BLET (go (maxArg+1) v) (go (maxArg+1) b)
        BAPP f x -> BAPP (go maxArg f) (go maxArg x)
        BVAR v   -> BVAR v
        BBAD v   -> BBAD v
        BCNS v   -> if isValCodeShaped maxArg v then BCNS v else BBAD v

-- 0, 1, ...
-- (0 f x)
-- (1 v b)
-- (2 c)
isValCodeShaped :: Nat -> Val Pln -> Bool
isValCodeShaped maxArg = loop
  where
    loop = \case
        NAT n                 -> n <= maxArg
        APP (APP (NAT 0) _) _ -> True
        APP (APP (NAT 1) _) _ -> True
        APP (NAT 2)         _ -> True

        REF (plunAlias -> Just rv) -> loop rv
        REF _                      -> False

        APP (REF r) a         -> case plunAlias r of
                                     Just rv -> loop (rv `APP` a)
                                     Nothing -> False
        APP (APP (REF r) a) b -> case plunAlias r of
                                     Just rv -> loop (rv `APP` a `APP` b)
                                     Nothing -> False

        _                     -> False

bodVal :: Bod a -> Val a
bodVal = \case
    BVAR v   -> NAT (fromIntegral v)
    BCNS c   -> APP (NAT 2) c
    BBAD v   -> v
    BAPP a b -> NAT 0 `APP` bodVal a `APP` bodVal b
    BLET v k -> NAT 1 `APP` bodVal v `APP` bodVal k

valBod :: Nat -> Val a -> Bod a
valBod maxArg = \case
    NAT v | v<=maxArg     -> BVAR v
    APP (NAT 2) v         -> BCNS v
    APP (APP (NAT 0) f) x -> BAPP (valBod (maxArg) f)   (valBod (maxArg) x)
    APP (APP (NAT 1) x) b -> BLET (valBod (maxArg+1) x) (valBod (maxArg+1) b)
    v                     -> BBAD v

{-
okIdn :: Text -> Bool
okIdn txt =
    fromMaybe False $ do
        (c, _ ) <- T.uncons txt
        guard (not (C.isDigit c))
        let parts = T.splitOn "-" txt
        guard (not $ null parts)
        guard (all okIdnFragment parts)
        pure True
  where
    okIdnFragment :: Text -> Bool
    okIdnFragment "" = False
    okIdnFragment tx = all okIdnChar tx

    okIdnChar '_' = True
    okIdnChar c   = C.isAlphaNum c
-}

nameClosure :: Closure -> NamedClosure
nameClosure (CLOSURE env val) =
    NAMED_CLOSURE names envir value
  where
    toMap :: (Int, (Pin, Maybe (Val Int)))
          -> Maybe (Symb, (Val Symb, ByteString))
    toMap (i, (p, mV)) = do
        v <- mV
        pure (names!i, ((names!) <$> v, pinHash p))

    value = (names!) <$> val
    names = assignNames (P.valTag . pinItem . fst <$> env)
    envir = mapFromList $ catMaybes
                        $ fmap toMap
                        $ toList
                        $ V.imap (,) env

assignNames :: Vector Symb -> Vector Symb
assignNames initialNms =
    evalState (traverse f initialNms) (mempty :: Map Symb Nat)
  where
    f :: Symb -> State (Map Symb Nat) Symb
    f nm = do
        tab :: Map symb Nat <- get
        let used = fromMaybe 1 (lookup nm nmCount)
        let sufx = fromMaybe 1 (lookup nm tab)
        let rend = let ?rexColors = NoColors
                   in rexLine (keyRex nm)
        put (insertMap nm (sufx+1) tab)

        let taken :: Nat -> Bool
            taken c = member c nmCount || member c tab

        let loop :: Nat -> State (Map Symb Nat) Symb
            loop n = do
                let candidate = utf8Nat (rend <> "_" <> tshow n)
                if (taken candidate)
                then loop (n+1)
                else do put $ insertMap nm (n+1)
                            $ insertMap candidate 1
                            $ tab
                        pure candidate

        if used==1
            then pure nm
            else loop sufx

    nmCount :: Map Symb Int
    nmCount = foldr (alterMap (Just . maybe 1 succ)) mempty initialNms

{-
            ( 1 , "" ) -> "_"
            ( 1 , _  ) -> nm
            ( _ , "" ) -> "_/" <> tshow sufx
            ( _ , _  ) -> nm <> "/" <> tshow sufx
            -- TODO Can't be a string.
-}

-- Plun Backend ----------------------------------------------------------------

type Load = State (Int, Map ByteString Int, [(Pin, Maybe (Val Int))])

valPlun :: Val Pln -> Pln
valPlun (NAT a)          = AT a
valPlun (REF v)          = v
valPlun (APP f x)        = valPlun f %% valPlun x
valPlun (BAR b)          = P.mkBar b
valPlun (ROW r)          = P.mkRow (toList $ valPlun <$> r)
valPlun (LAW n a b)      = P.mkLaw n a (valPlun $ bodVal b)
valPlun (COW 0)          = P.nodVal $ P.DAT $ P.ROW mempty
valPlun (COW n)          = P.nodVal $ P.DAT $ P.COW n
valPlun (TAB t)          = P.nodVal $ P.DAT $ P.TAB $ fmap valPlun t
valPlun (CAB k) | null k = P.nodVal $ P.DAT $ P.TAB mempty
valPlun (CAB k)          = P.nodVal $ P.DAT $ P.CAB k

plunAlias :: Pln -> Maybe (Val Pln)
plunAlias (PLN _ (P.DAT P.PIN{})) = Nothing
plunAlias (PLN _ topNod)  = Just (nod topNod)
  where
    go (PLN _ n) = nod n
    nod = \case
        P.NAT a           -> NAT a
        n@(P.DAT P.PIN{}) -> REF (P.nodVal n)
        P.DAT (P.BAR b)   -> BAR b
        P.DAT (P.ROW r)   -> ROW (go <$> r)
        P.DAT (P.TAB t)   -> TAB (go <$> t)
        P.DAT (P.COW n)   -> COW n
        P.DAT (P.CAB n)   -> CAB n
        P.APP f x         -> APP (nod f) (go x)
        P.FUN P.L{..}     -> LAW lawName lawArgs (valBod lawArgs $ go lawBody)

loadShallow :: Pln -> Closure
loadShallow inVal =
    let (top, (_,_,stk)) = runState (goTop inVal) (0, mempty, [])
    in CLOSURE (fromList $ reverse stk) top
  where
    goTop :: Pln -> Load (Val Int)
    goTop (PLN _ (P.DAT (P.PIN i))) = REF <$> goTopPin i
    goTop vl                        = go vl

    goTopPin :: P.Pin -> Load Int
    goTopPin pin@P.P{..} = do
        kor <- (pin,) . Just <$> goTop pinItem
        (nex, tab, stk) <- get
        let tab' = insertMap pinHash nex tab
        put (nex+1, tab', kor:stk)
        pure nex

    go :: Pln -> Load (Val Int)
    go (PLN _ nod) =
        case nod of
            P.NAT a         -> pure (NAT a)
            P.DAT (P.PIN b) -> REF <$> goPin b
            P.DAT (P.BAR b) -> pure (BAR b)
            P.DAT (P.ROW r) -> ROW <$> traverse go r
            P.DAT (P.TAB t) -> TAB <$> traverse go t
            P.DAT (P.COW n) -> pure (COW n)
            P.DAT (P.CAB n) -> pure (CAB n)
            P.APP f x       -> APP <$> go (P.nodVal f) <*> go x
            P.FUN (P.L{..}) -> do
                b <- go lawBody
                pure $ LAW lawName lawArgs (valBod lawArgs b)

    goPin :: P.Pin -> Load Int
    goPin pin = do
        (_, tabl, _) <- get
        case lookup (pinHash pin) tabl of
            Just i -> pure i
            Nothing -> do
                let kor = (pin, Nothing)
                (nex, tab, stk) <- get
                let tab' = insertMap (pinHash pin) nex tab
                put (nex+1, tab', kor:stk)
                pure nex

loadClosure :: Pln -> Closure
loadClosure inVal =
    let (top, (_,_,stk)) = runState (go inVal) (0, mempty, [])
    in CLOSURE (fromList $ reverse stk) top
  where
    go :: Pln -> Load (Val Int)
    go (PLN _ nod) =
        case nod of
            P.NAT a         -> pure (NAT a)
            P.DAT (P.PIN b) -> REF <$> goPin b
            P.DAT (P.BAR b) -> pure (BAR b)
            P.DAT (P.ROW r) -> ROW <$> traverse go r
            P.DAT (P.TAB t) -> TAB <$> traverse go t
            P.DAT (P.COW n) -> pure (COW n)
            P.DAT (P.CAB n) -> pure (CAB n)
            P.APP f x       -> goCel (P.nodVal f) x
            P.FUN (P.L{..}) -> do
                b <- go lawBody
                pure $ LAW lawName lawArgs (valBod lawArgs b)

    goCel x y = APP <$> go x <*> go y

    goPin :: P.Pin -> Load Int
    goPin pin = do
        (_, tabl, _) <- get
        case lookup (pinHash pin) tabl of
            Just i -> pure i
            Nothing -> do
                kor <- (pin,) . Just <$> go (pinItem pin)
                (nex, tab, stk) <- get
                let tab' = insertMap (pinHash pin) nex tab
                put (nex+1, tab', kor:stk)
                pure nex

-- 0, 1, ...
-- (0 f x)
-- (1 v b)
-- (2 c)
isPlnCodeShaped :: Nat -> Pln -> Bool
isPlnCodeShaped maxArg = loop
  where
    loop = \case
        AT n              -> n <= maxArg
        P.NAT 0 :& _ :# _ -> True
        P.NAT 1 :& _ :# _ -> True
        P.NAT 2 :# _      -> True
        _                 -> False

-- TODO Better to return (Val P.Pin), but dont want to add another
-- type varibale to the Backend abstration.  Once we kill the abstraction,
-- then we should simplify this.
plunVal :: Pln -> Val Pln
plunVal = goVal
  where
    goVal (PLN _ n) = goNod n

    goNod :: P.Nod -> Val Pln
    goNod = \case
        P.NAT a            -> NAT a
        P.APP x y          -> APP (goNod x) (goVal y)
        P.FUN P.L{..}      -> LAW lawName lawArgs
                                  $ valBod lawArgs
                                  $ goVal lawBody
        vl@(P.DAT P.PIN{}) -> REF (P.nodVal vl)
        P.DAT (P.BAR b)    -> BAR b
        P.DAT (P.COW n)    -> COW n
        P.DAT (P.CAB ks)   -> CAB ks
        P.DAT (P.ROW xs)   -> ROW (goVal <$> xs)
        P.DAT (P.TAB t)    -> TAB (goVal <$> t)


-- Hacked Together Snapshotting ------------------------------------------------

hashPath :: FilePath -> ByteString -> FilePath
hashPath dir haz =
    (dir <> "/" <> (unpack $ encodeBtc haz))

plunSave :: MonadIO m => FilePath -> Pln -> m ByteString
plunSave dir = liftIO . \case
    PLN _ (P.DAT (P.PIN p)) -> savePin p
    vl                      -> P.mkPin' vl >>= savePin
  where
    savePin (P.P{..}) = do
        createDirectoryIfMissing True dir

        let pax = hashPath dir pinHash
        exists <- doesFileExist pax
        unless exists $ do
            for_ pinRefs (plunSave dir . P.nodVal . P.DAT . P.PIN)
            unless (null pinRefs) $ do
                let dep = pax <> ".deps"
                putStrLn ("WRITE FILE: " <> pack dep)
                writeFile dep $ concat $ fmap P.pinHash pinRefs
            putStrLn ("WRITE FILE: " <> pack pax)
            writeFile pax pinBlob

        pure pinHash

plunLoad :: MonadIO m => FilePath -> ByteString -> m Pln
plunLoad dir key = liftIO do
    book <- P.stBook <$> readIORef P.state
    case lookup key book of
        Just rl -> pure $ P.nodVal $ P.DAT $ P.PIN rl
        Nothing -> loadSnapshotDisk dir key

loadDeps :: FilePath -> ByteString -> IO (Vector ByteString)
loadDeps dir key = do
    let pax = hashPath dir key <> ".deps"
    doesFileExist pax >>= \case
        False -> pure mempty
        True  -> fromList . parseDeps <$> readFile pax
  where
    parseDeps :: ByteString -> [ByteString]
    parseDeps bs | null bs = []
    parseDeps bs           = take 32 bs : parseDeps (drop 32 bs)

loadSnapshotDisk :: FilePath -> ByteString -> IO Pln
loadSnapshotDisk dir key = do
    createDirectoryIfMissing True dir
    let pax = hashPath dir key
    rfhz <- loadDeps dir key
    bytz <- readFile pax
    refs <- for rfhz (plunLoad dir)
    valu <- capBSExn refs bytz
    evaluate (force $ P.mkPin valu)
