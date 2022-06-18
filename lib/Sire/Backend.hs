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

module Sire.Backend
    ( Closure(..)
    , NamedClosure(..)
    , nameClosure
    , rulePlun
    , makeShallow
    , valBod
    , bodVal
    , optimizeRul
    , Pln
    , plunSave
    , plunLoad
    , valPlun
    , plunVal
    , plunClosure
    , plunShallow
    )
where

import PlunderPrelude
import Sire.Types
import System.Directory
import Plun.Print

import Control.Monad.State  (State, evalState, runState, get, put)
import Data.Vector          ((!))
import Jar                  (capBSExn)
import Plun                 (pattern AT, (%%))

import qualified Data.Char   as C
import qualified Data.Text   as T
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
data Closure a = CLOSURE
    { closureEnv :: Vector (a, Either Nat (Val Int))
    , closureVal :: Val Int
    }
  deriving (Show, Generic, NFData)

data NamedClosure = NAMED_CLOSURE
    { nmz :: Vector Text
    , env :: Map Text (Val Text)
    , val :: Val Text
    }
  deriving (Show, Generic, NFData)

valName :: Val a -> LawName
valName (LAW n _ _) = n
valName _           = LN 0

--  This is a hack to make a full-load look like a shallow load.
--  Backends should support actual shallow loads for performance reasons,
--  but this is a stop-gap hack on the way towards building that out.
makeShallow :: Closure val -> Closure val
makeShallow (CLOSURE env top) = CLOSURE env' top
  where
    env' = (flip V.imap env) \i (raw, ev) ->
             (raw,) $
             case ev of
               Right v | REF i == top -> Right v
               Right v                -> Left (lawNameNat $ valName v)
               Left nm                -> Left nm

rulePlun :: Rul Pln -> Pln
rulePlun = valPlun . rulVal . optimizeRul

rulVal :: Rul a -> Val a
rulVal (RUL (LN nm) ar bd) =
    NAT 0 `APP` (NAT $ fromIntegral nm)
          `APP` (NAT ar)
          `APP` (bodVal bd)

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
        BCNS v   -> if isCodeShaped maxArg v then BCNS v else BBAD v

-- 0, 1, ...
-- (0 f x)
-- (1 v b)
-- (2 c)
isCodeShaped :: Nat -> Val Pln -> Bool
isCodeShaped maxArg = loop
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

initialIdn :: Either Nat (Val Int) -> Text
initialIdn = natIdn . either id (lawNameNat . valName)

natIdn :: Nat -> Text
natIdn name = fromMaybe "" $ do
    txt <- either (const Nothing) Just (natUtf8 name)
    guard (okIdn txt)
    pure txt

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

nameClosure :: Closure v -> NamedClosure
nameClosure (CLOSURE env val) =
    NAMED_CLOSURE names envir value
  where
    toMap :: (Int, Either Nat (Val Int)) -> Maybe (Text, Val Text)
    toMap (_, Left _)  = Nothing
    toMap (i, Right v) = Just (names!i, fmap (names!) v)

    envir = mapFromList
          $ catMaybes
          $ fmap toMap
          $ toList
          $ V.imap (,)
          $ fmap snd env

    value = (names!) <$> val
    names = assignNames (initialIdn . snd <$> env)

assignNames :: Vector Text -> Vector Text
assignNames initialNms =
    evalState (traverse f initialNms) (mempty :: Map Text Int)
  where
    f :: Text -> State (Map Text Int) Text
    f nm = do
        tab <- get
        let used = fromMaybe 1 (lookup nm nmCount)
        let sufx = fromMaybe 1 (lookup nm tab)
        put (insertMap nm (sufx+1) tab)
        pure $ case (used, nm) of ( 1 , "" ) -> "_"
                                  ( 1 , _  ) -> nm
                                  ( _ , "" ) -> "_/" <> tshow sufx
                                  ( _ , _  ) -> nm <> "/" <> tshow sufx
                                             -- TODO Can't be a string.

    nmCount :: Map Text Int
    nmCount = foldr (alterMap (Just . maybe 1 succ)) mempty initialNms

-- Plun Backend ----------------------------------------------------------------

type Pln = P.Val

type GetPlun v = State (Int, Map ByteString Int, [(v, Val Int)])

valPlun :: Val P.Val -> P.Val
valPlun (NAT a)     = AT a
valPlun (REF v)     = v
valPlun (APP f x)   = valPlun f %% valPlun x
valPlun (BAR b)     = P.mkBar b
valPlun (ROW r)     = P.mkRow (toList $ valPlun <$> r)
valPlun (LAW n a b) = P.mkLaw n a (valPlun $ bodVal b)
valPlun (COW n)     = P.nodVal $ P.DAT $ P.COW n
valPlun (TAB t)     = P.nodVal $ P.DAT $ P.TAB $ (valPlun <$> t)
valPlun (CAB k)     = P.nodVal $ P.DAT $ P.CAB k

plunAlias :: P.Val -> Maybe (Val P.Val)
plunAlias (P.VAL _ P.PIN{}) = Nothing
plunAlias (P.VAL _ topNod)  = Just (nod topNod)
  where
    go (P.VAL _ n) = nod n
    nod = \case
        n@P.PIN{}       -> REF (P.nodVal n)
        P.NAT a         -> NAT a
        P.DAT (P.BAR b) -> BAR b
        P.DAT (P.ROW r) -> ROW (go <$> r)
        P.DAT (P.TAB t) -> TAB (go <$> t)
        P.DAT (P.COW n) -> COW n
        P.DAT (P.CAB n) -> CAB n
        P.APP f x       -> APP (nod f) (go x)
        P.LAW P.L{..}   -> LAW lawName lawArgs (valBod lawArgs $ go lawBody)

-- TODO This is very slow, directly implementing shallow load should
-- be pretty easy.  Just do it.
plunShallow :: Pln -> Closure Pln
plunShallow = makeShallow . plunClosure

plunClosure :: P.Val -> Closure P.Val
plunClosure inVal =
    let (top, (_,_,stk)) = runState (go inVal) (0, mempty, [])
    in CLOSURE (fmap (over _2 Right) $ fromList $ reverse stk) top
  where
    go :: P.Val -> GetPlun P.Val (Val Int)
    go (P.VAL _ nod) =
        case nod of
            P.NAT a         -> pure (NAT a)
            P.DAT (P.BAR b) -> pure (BAR b)
            P.DAT (P.ROW r) -> ROW <$> traverse go r
            P.DAT (P.TAB t) -> TAB <$> traverse go t
            P.DAT (P.COW n) -> pure (COW n)
            P.DAT (P.CAB n) -> pure (CAB n)
            P.APP f x       -> goCel (P.nodVal f) x
            P.PIN b         -> REF <$> goPin b
            P.LAW (P.L{..}) -> do
                b <- go lawBody
                pure $ LAW lawName lawArgs (valBod lawArgs b)

    goCel x y = APP <$> go x <*> go y

    goPin :: P.Pin -> GetPlun P.Val Int
    goPin P.P{..} = do
        (_, tabl, _) <- get
        case lookup pinHash tabl of
            Just i -> pure i
            Nothing -> do
                kor <- (pinItem,) <$> go pinItem
                (nex, tab, stk) <- get
                let tab' = insertMap pinHash nex tab
                put (nex+1, tab', kor:stk)
                pure nex

-- TODO Better to return (Val P.Pin), but dont want to add another
-- type varibale to the Backend abstration.  Once we kill the abstraction,
-- then we should simplify this.
plunVal :: P.Val -> Val P.Val
plunVal = goVal
  where
    goVal (P.VAL _ n) = goNod n

    goNod :: P.Nod -> Val P.Val
    goNod = \case
        vl@P.PIN{}       -> REF (P.nodVal vl)
        P.NAT a          -> NAT a
        P.APP x y        -> APP (goNod x) (goVal y)
        P.LAW P.L{..}    -> LAW lawName lawArgs
                                $ valBod lawArgs
                                $ goVal lawBody
        P.DAT (P.BAR b)  -> BAR b
        P.DAT (P.COW n)  -> COW n
        P.DAT (P.CAB ks) -> CAB ks
        P.DAT (P.ROW xs) -> ROW (goVal <$> xs)
        P.DAT (P.TAB t)  -> TAB (goVal <$> t)


-- Hacked Together Snapshotting ------------------------------------------------

hashPath :: FilePath -> ByteString -> FilePath
hashPath dir haz =
    (dir <> "/" <> (unpack $ encodeBtc haz))

plunSave :: MonadIO m => FilePath -> P.Val -> m ByteString
plunSave dir = liftIO . \case
    P.VAL _ (P.PIN p) -> savePin p
    vl                -> P.mkPin' vl >>= savePin
  where
    savePin (P.P{..}) = do
        createDirectoryIfMissing True dir

        let pax = hashPath dir pinHash
        exists <- doesFileExist pax
        unless exists $ do
            for_ pinRefs (plunSave dir . P.nodVal . P.PIN)
            unless (null pinRefs) $ do
                let dep = pax <> ".deps"
                putStrLn ("WRITE FILE: " <> pack dep)
                writeFile dep $ concat $ fmap P.pinHash pinRefs
            putStrLn ("WRITE FILE: " <> pack pax)
            writeFile pax pinBlob

        pure pinHash

plunLoad :: MonadIO m => FilePath -> ByteString -> m P.Val
plunLoad dir key = liftIO do
    book <- P.stBook <$> readIORef P.state
    case lookup key book of
        Just rl -> pure $ P.nodVal (P.PIN rl)
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

loadSnapshotDisk :: FilePath -> ByteString -> IO P.Val
loadSnapshotDisk dir key = do
    createDirectoryIfMissing True dir
    let pax = hashPath dir key
    rfhz <- loadDeps dir key
    bytz <- readFile pax
    refs <- for rfhz (plunLoad dir)
    valu <- capBSExn refs bytz
    evaluate (force $ P.mkPin valu)
