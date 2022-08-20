{-# OPTIONS_GHC -Wall   #-}
{-# OPTIONS_GHC -Werror #-}

{-

Alright, so the longer term plan here, once the Loot codebase stabilizes,
is to:

    - Remove all printing from this file.
    - Rely on Loot parser for loot things.
    - Rely on Loot printer for REPL output.
    - Remove all the Sugar and move it into macros.
-}

{-
Inlining
========

TODO: Implement inlining

-   Given a function to inline: (Fun Fan Refr Fan)
-   Given an array of arguments: (Vector (Exp Fan Refr Fan))
-   Presuming that the number of arguments matches the function arity.
-   Return (Exp Fan Refr Fan)
-   Each function argument becomes a let bindings.
-   Function body simply becomes let-binding body.

TODO: Figure how coherent system of runes between Loot and Sire.

TODO: Implement static-application rune.

-   Something like [: f x y]
-   Try to convert f and x and y to static values.
-   To do this properly, we need to track all local bindings.  (Do they
    depend on variables?)
-   Validate that `f` is an inlinable quantity.
-   Validate arity.
-   Perform inlining.

TODO: Implement inline binders.

-   Something like [! f x]=(x x)
-   Same behavior as [f x]=(x x)
-   (Fun Fan Refr Fan) added to "inline table".
-   A compiler pass recognizes application to inline binder, and replaces
    it with explicit inline-application.

TODO: Implement inline lambdas

-   Something like ((f x &! (x x)) 9)
-   Compiler recognizes application to inline lambda, and replaces with
    inline-application of normal lambda.

TODO: Implement compile-time-lambda binders.

-   Something like [: f x]=(x x)
-   Same behavior as [f x]=(x x)
-   (Fun Fan Refr Fan) added to "static-lambda table".
-   A compiler pass recognizes application to inline binder, and replaces
    it with explicit compiler-time-application of normal lambda.

TODO: Implement compile-time lambdas.

-   Something like ((f x &: x x) 9)
-   Compiler recognizes application to inline lambda, and replaces with
    compile-time-application of normal lambda.


    Rex Representation in Plunder
    =============================

    Representation could be:

    %fdsa ;; bare_word

    {rune}
    {rune kids}
    {rune kids cont}

    {1 text}
    {1 text style}
    {1 text style cont}

    {2 embed}
    {2 embed cont}
-}

module Sire.Syntax
    ( readCmd
    , rexCmd
    , plunderMacro
    , MacroEnv(..)
    )
where

import PlunderPrelude
import Rex
import Loot.Types (Val(..), XTag(xtagIdn), xtagTag, LawName(..))
import Sire.Types
import Loot.Backend (valPlun, plunVal)

import Loot.Syntax (readIdnTxt, readKey, readSymb, readBymb)
import Loot.Syntax (readArgs, readSigy)
import Loot.Syntax (readNat)
import Loot.Syntax (readXTag, simpleTag)
-- ort Loot.Syntax (rForm1Nc, rFormNc)
import Loot.Syntax (rForm1Nc)
import Loot.Syntax (rForm2c, rForm1, rForm1c, rFormN1c, rForm3c)
import Sire.Macro  (rexVal, loadRex, loadRow)
import Plun.Print  (dent)

import qualified Loot.Syntax  as Loot
import qualified Loot.Sugar   as Loot
import qualified Loot.ReplExe as Loot


-- Types -----------------------------------------------------------------------

data MacroEnv = MacroEnv
    { meGenSymState :: IORef Nat
    , meGlobalScope :: Fan
    , meMacros      :: Map Text Fan
    }

type MacroExpander v
    = v
    -> Nat
    -> [(GRex v)]
    -> Maybe (GRex v)
    -> IO (Either (GRex v, Text) (Nat, (GRex v)))

type HasMacroEnv = (?macros :: MacroEnv)

type Red = ReadT Fan IO


-- Parsing ---------------------------------------------------------------------

rexCmd :: HasMacroEnv => Rex -> Either Text XCmd
rexCmd rex =
    load (preProcess rex)
 where
  preProcess :: Rex -> GRex v
  preProcess = rewriteLeafJuxtaposition . fmap absurd

  load :: GRex Fan -> Either Text XCmd
  load = Loot.resultEitherText dropEmbed rex
       . runReading readCmd

  dropEmbed :: Show v => GRex v -> Rex
  dropEmbed = \case
      T s t k     -> T s t                    (dropEmbed <$> k)
      C c k       -> T THIN_CORD (tshow c)    (dropEmbed <$> k)
      N m r xs mK -> N m r (dropEmbed <$> xs) (dropEmbed <$> mK)

-- TODO: Should the parser just handle this directly?
rewriteLeafJuxtaposition :: GRex a -> GRex a
rewriteLeafJuxtaposition = go
  where
    go = \case
      T s t Nothing  -> T s t Nothing
      T s t (Just x) -> N SHUT_INFIX "-" [T s t Nothing, go x] Nothing
      N m r p k      -> N m r (go <$> p) (go <$> k)
      C x k          -> C x (go <$> k)

runReading :: Red a -> GRex Fan -> Result Fan a
runReading act = unsafePerformIO . runResultT . runReadT act

withRex :: Red v -> Red (Rex, v)
withRex red = (,) <$> getRex <*> red
  where
   getRex = fmap (const $ error "impossible") <$> readRex

readImports :: Red [(Text, Set Symb)]
readImports = do
    rune "/+"
    asum
        [ do i <- form1 readIdnTxt
             pure [(i, mempty)]
        , do (i,xs) <- form1C readIdnTxt readImports
             pure ((i,mempty):xs)
        , do (i,rs) <- form2 readIdnTxt nameList
             pure [(i, setFromList rs)]
        , do (i,rs,xs) <- form2C readIdnTxt nameList readImports
             pure ((i, setFromList rs):xs)
        ]
  where
    nameList = (singleton <$> readKey) <|> (rune "|" >> formN readKey)

readFilter :: Red [Symb]
readFilter = do
    rune "^-^"
    formNe readKey readFilter <&> \case
        (is, Nothing)   -> is
        (is, Just more) -> is <> more

readCmd :: HasMacroEnv => Red XCmd
readCmd = asum
    [ rune "=" >> readDefine
    , IMPORT <$> readImports
    , FILTER <$> readFilter
    , do rune "??"
         checks <- slip1N "??" (withRex readExpr) (withRex readExpr)
         let bld :: ((Rex,XExp),[(Rex,XExp)]) -> ([Rex],XExp)
             bld (v,vs) =  ( fst v : (fst <$> vs)
                           , foldl' EAPP (snd v) (snd <$> vs)
                           )
         pure $ CHECK $ fmap bld checks
    , do rune "<"
         (v,vs) <- form1Nc readExpr readExpr
         pure (DUMPY $ foldl' EAPP v vs)
    , do rune "<-"
         let readIdns = rune "," >> form2 readSymb readSymb
         ((rid,res), exr) <- form2 readIdns readExpr
         pure (IOEFF rid res exr)
    , SAVEV <$> (rForm1 "<<" readExpr id)
    , rForm1c "#?" readExpr EPLOD
    , OUTPUT <$> readExpr
    , do rune "!*!"
         fmap EFFECT $ asum $
             [ KAL_LIST_REQUESTS <$ form0
             , KAL_CANCEL_REQUEST <$> form1 readKey
             , uncurry KAL_MAKE_REQUEST <$> form2 readKey readExpr
             ]
    , do rune "?*?"
         fmap EFFECT $ asum $
             [ KAL_LIST_RESPONSES <$ form0
             , KAL_DELETE_RESPONSE <$> form1 readKey
             ]
    ]
  where
    readDefine = do
        res <- slip2 "=" readBinder readExpr
        pure $ DEFINE $ res <&> \((t,args),b) ->
            let nm = xtagIdn t
                tg = xtagTag t
            in
                case args of
                    []   -> BIND_EXP nm b
                    r:rs -> BIND_FUN nm (FUN nm (LN tg) (r:|rs) b)


-- Functions -------------------------------------------------------------------

-- rexFanRex :: RexColor => GRex Fan -> GRex v
-- rexFanRex = fmap absurd
          -- . Loot.joinRex
          -- . fmap Loot.plunRex

valFanRex :: RexColor => Val Fan -> GRex v
valFanRex = fmap absurd
          . Loot.joinRex
          . Loot.valRex
          . Loot.resugarVal mempty
          . fmap (utf8Nat . rexLine . Loot.plunRex)

-- TODO Better to just use a parameter instead of IO monad?
-- TODO This is non-generic now, can we just collapse all this complexity?
runMacro :: HasMacroEnv => MacroExpander Fan -> Red XExp
runMacro macro = do
    (xs, mK) <- readNode
    let MacroEnv{..} = ?macros
    n <- readIORef meGenSymState
    liftIO (macro meGlobalScope n xs mK) >>= \case
        Left (rex, msg) ->
            let ?rexColors = NoColors in
            throwError $ (msg <>)
                       $ ("\n\nIn Sub-Expression:\n\n" <>)
                       $  dent "   "
                       $ rexFile
                       $ Loot.joinRex
                       $ fmap Loot.plunRex rex
        Right (!used, !res) -> do
            modifyIORef' meGenSymState (+used)
            liftIO ( runResultT
                   $ runReadT readExpr res
                   ) >>= readResult

readExpr :: HasMacroEnv => Red XExp
readExpr = do
    let MacroEnv{meMacros} = ?macros
    let macros = mapToList meMacros <&> \(r,h) -> do rune r
                                                     runMacro (plunderMacro h)
    asum (fixed <> macros)
  where
    fixed =
        [ EBED <$> readExtra
        , rune "%%" >> readOpenTabish
        , do rune "%"
             form1 $ asum
                 [ ENAT . utf8Nat <$> Loot.readName
                 , either ECAB ETAB <$> readTab readExpr EREF
                 ]
        , ENAT <$> readNat
        , EREF <$> readSymb
        , rFormN1c "."   readExpr readExpr          appTo
        , rForm3c  "@@"  readSymb readExpr readExpr EREC
        , rForm2c  "?"   readSigy readExpr          nameLam
        , rForm2c  "&"   readArgs readExpr          anonLam

        -- TODO Make this a macro
        , rFormN1c "^"   readExpr readTBin          mkWhere

        , rForm1Nc "|"   readExpr readExpr          apple
        , rForm1Nc "-"   readExpr readExpr          apple
        , rForm3c  "@"   readSymb readExpr readExpr ELET
        , rForm1Nc "!"   readExpr readExpr          mkInline
        ]

    apple f []    = f
    apple f (b:c) = apple (EAPP f b) c

    appTo xs f = apple f xs

    mkInline f xs = ELIN (f :| xs)

    mkWhere :: [XExp] -> [(Symb, XExp)] -> XExp
    mkWhere []     []         = ENAT 0 -- TODO: Implement and use `form1N1c`
    mkWhere (f:xs) []         = apple f xs
    mkWhere fxs    ((n,v):bs) = ELET n v (mkWhere fxs bs)

    anonLam :: NonEmpty Symb -> XExp -> XExp
    anonLam rs x = ELAM (FUN 0 (LN 0) rs x)

    nameLam :: (XTag, NonEmpty Symb) -> XExp -> XExp
    nameLam (t,rs) x = ELAM (FUN (xtagIdn t) (LN $ xtagTag t) rs x)

    readTBin = rune "=" >> slip2 "=" readSymb readExpr

-- TODO Make this a macro.
readTab :: ∀a. Red a -> (Symb -> a) -> Red (Either (Set Nat) (Map Nat a))
readTab ele var = do
    rune ","

    let keyEle n = (n, var n)

    res :: [Either Nat (Nat,a)]
        <- formN $ asum
                 [ Left <$> Loot.readKey
                 , Right <$> (rune "=" >> (    form2 readKey ele
                                           <|> (keyEle <$> form1 readKey)
                                          ))
                 ]

    let goRyt acc []                              = pure (Right acc)
        goRyt _   (Left _     :_)                 = sawBoth
        goRyt acc (Right (k,_):_)  | member k acc = dups k
        goRyt acc (Right (k,v):ks) | otherwise    = goRyt (insertMap k v acc) ks

    let goLef acc []                          = pure (Left acc)
        goLef _   (Right _:_)                 = sawBoth
        goLef acc (Left k :_)  | member k acc = dups k
        goLef acc (Left k :ks) | otherwise    = goLef (insertSet k acc) ks

    case res of
      []               -> pure (Right mempty)
      Right (k,v) : es -> goRyt mempty        (Right (k,v) : es)
      Left  e     : es -> goLef (singleton e) es
  where
    sawBoth = throwError "Cannot mix %{a} and %{a=3} forms."
    dups k  = throwError ("Key appears twice in tab literal: " <> tshow k)


-- Macros ----------------------------------------------------------------------

-- TODO This can be massivly simplified.
plunderMacro :: Fan -> MacroExpander Fan
plunderMacro macroLaw macEnv nex xs mK = do
    let vs  = ROW $ fromList (rexVal <$> xs)
    let kv  = maybe (NAT 0) rexVal mK
    let res = valPlun
                ( REF macroLaw
                    `APP` REF macEnv
                    `APP` NAT nex
                    `APP` vs
                    `APP` kv)

    let vl = plunVal res
    pure $ let ?rexColors = NoColors
           in loadMacroExpansion vl

loadMacroExpansion
    :: RexColor
    => Val Fan
    -> Either (GRex Fan, Text) (Nat, GRex Fan)
loadMacroExpansion topVal = case topVal of
    APP (NAT 0) errRow ->
        case loadRow errRow of
            Left (val, le) ->
                Left (valFanRex val, loadErr errRow "error context" le)
            Right [errExp, NAT err] -> do
                case (loadRex valPlun errExp, natUtf8Exn err) of
                    (Left (val, _le), ctx) -> Left (valFanRex val, ctx)
                    (Right rez, errMsg)   -> Left (rez, errMsg)
                    -- TODO handle invalid error strings more carefully.
            _ -> topBad
    APP (NAT 1) expRow ->
        case loadRow expRow of
            Left (val, le) ->
                Left (valFanRex val, loadErr expRow "result" le)
            Right [NAT used, body] -> do
                case (loadRex valPlun body) of
                    Left (val, err) -> Left (valFanRex val, err)
                    Right rez -> Right (used, rez)
            _ -> topBad
    _ -> topBad
  where
    loadErr :: Val Fan -> Text -> Text -> Text
    loadErr _val ctx expect = concat
        [ "Error when loading "
        , ctx
        , ".  Was expecting: "
        , expect
        ]

    bad expr expLn =
        unlines
            (["Invalid macro expansion.  Expected " <> expr] <> expLn)

    topBad = Left (valFanRex topVal, bad "one of:" shapes)
      where
        shapes =
            [ ""
            , "    (0 [val/Any error/Nat])"
            , ""
            , "    (1 [used/Nat exp/Rex])"
            ]

readBinder :: Red (XTag, [Symb])
readBinder = simple <|> complex
  where
    simple = fmap ((,[]) . simpleTag) readBymb

    -- TODO Indicate inline-ness
    complex = do
        rune "|" <|> rune "-" <|> rune "!"
        form1N readXTag readSymb

-- TODO Tabs now use `=key` an cabs just use `key`.
readOpenTabish :: HasMacroEnv => Red XExp
readOpenTabish = do
    rex <- readRex
    case rex of
        N _ "%%" (N _ "=" [_] _ : _) _ -> ETAB <$> readOpenTab
        _                              -> ECAB <$> Loot.readOpenCab

readOpenTab :: HasMacroEnv => Red (Map Nat XExp)
readOpenTab = do
    pairs <- slip1N "%%" (rune "=" >> form1 readKey) readExpr
    let keySet = setFromList (fst <$> pairs) :: Set Nat
    when (length pairs /= length keySet) do
        -- TODO Find and show the offending key.
        throwError "Duplicated Key"
    pure $ mapFromList
         $ turn pairs
         $ \case (k, [])   -> (k, EREF k)
                 (k, x:xs) -> (k, foldl' EAPP x xs)
