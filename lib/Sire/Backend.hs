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
    ( Backend(..)
    , Closure(..)
    , NamedClosure(..)
    , nameClosure
    , injectRul
    , makeShallow
    , valBod
    , bodVal
    , optimizeRul
    )
where

import PlunderPrelude
import Sire.Types

import Control.Monad.State (State, evalState, get, put)
import Data.Vector         ((!))

import qualified Data.Char   as C
import qualified Data.Text   as T
import qualified Data.Vector as V

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

data Backend m val = BACKEND
    { bArity    :: val -> m Nat
    , bPutVal   :: Val val -> m val
    , bMkPin    :: val -> m val
    , bClosure  :: val -> m (Closure val)
    , bShallow  :: val -> m (Closure val)
    , bGetVal   :: val -> m (Val val)
    , bGetAlias :: val -> m (Maybe (Val val))
    , bDump     :: FilePath -> val -> m ByteString
    , bLoad     :: FilePath -> ByteString -> m val
    , bIO       :: ∀a. m a -> IO a
    , bIOErr    :: ∀a. m a -> IO (Either Text a)
    }

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

injectRul :: Show a => MonadError Text m => Backend m a -> Rul a -> m a
injectRul be dumbRule = do
    rul <- optimizeRul (bGetAlias be) dumbRule
    bPutVal be (rulVal rul)

rulVal :: Rul a -> Val a
rulVal (RUL (LN nm) ar bd) =
    NAT 0 `APP` (NAT $ fromIntegral nm)
          `APP` (NAT ar)
          `APP` (bodVal bd)

{-
    Removes extraneous BAD
-}
optimizeRul :: ∀a m. (Show a, Monad m) => (a -> m (Maybe (Val a))) -> Rul a -> m (Rul a)
optimizeRul loadAlias =
    \ (RUL nm ar bd)
    → RUL nm ar <$> go ar bd
         -- traceM $ show (rulBody inp)
         -- traceM $ show (rulBody res)
         -- pure res
  where
    go :: Nat -> Bod a -> m (Bod a)
    go maxArg = \case
        BLET v b -> BLET <$> go (maxArg+1) v <*> go (maxArg+1) b
        BAPP f x -> BAPP <$> go maxArg f <*> go maxArg x
        BVAR v   -> pure $ BVAR v
        BBAD v   -> pure $ BBAD v
        BCNS v   -> do
            isCodeShaped maxArg loadAlias v >>= \case
                False -> pure (BBAD v)
                True  -> pure (BCNS v)

-- 0, 1, ...
-- (0 f x)
-- (1 v b)
-- (2 c)
isCodeShaped :: Monad m => Nat -> (a -> m (Maybe (Val a))) -> Val a -> m Bool
isCodeShaped maxArg loadAlias = loop
  where
    loop = \case
        NAT n                 -> pure (n<=maxArg)
        APP (APP (NAT 0) _) _ -> pure True
        APP (APP (NAT 1) _) _ -> pure True
        APP (NAT 2)         _ -> pure True

        REF r                 -> loadAlias r >>= \case
                                     Just rv -> loop rv
                                     Nothing -> pure False
        APP (REF r) a         -> loadAlias r >>= \case
                                     Just rv -> loop (rv `APP` a)
                                     Nothing -> pure False
        APP (APP (REF r) a) b -> loadAlias r >>= \case
                                     Just rv -> loop (rv `APP` a `APP` b)
                                     Nothing -> pure False

        _                     -> pure False

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
