{-# OPTIONS_GHC -Wall   #-}
{-# OPTIONS_GHC -Werror #-}

{-

-   TODO Make parser match printer and test:

    -   TODO Test reparsing the output of running all of our Sire code.

    -   TODO Implement `0:1` Bod syntax in parser (already in printer)

-   TODO `*` should support both rows and tabs.

    -   Tabs look like: `[* %{a b} %{a=3 b=4} add-a-b]`.
    -   Rows look like: `[* {a b} {a=3 b=4} add-a-b]`.

-   TODO: This switches between wide and tall modes by repeatedly
    re-serializing.  This is insane!  It would be better if the printer
    could store this information on the rex nodes themselves and calculate
    that lazily.

-   TODO: Consider implementing generic pattern matching.

    `* pat val` expects a specific pattern (no check)

    `[+ exp][= pat exp][= pat exp]`

    That switches across patterns.

    Patterns may be any of:

        #
        [# _]
        {}
        {#}
        {_}
        {# _ ...}
        {_ _ ...}
        %{...}

    I expect this is too complex to be worth it, but it's an idea.

Rune Table:

    $   ----
    !   LIN (anonymus lambda to be inlined)
    #   COR (cores, for mutually recursive functions)
    %   TAB/NAT `%{tab='lit'}` / `%nat_lit`
    &   LAM (anonymous lambda)
    +   PAT (case expression, pattern matching)
    ,   ROW (vector literal)
    -   LET (non-recursive let)
    |   APP (function application)
    .   APP (reverse function application)
    /   ALIAS (cmd only, maybe should be multi-char?)
    `   BIG (full, optimized LETREC)
    =   misc (not Exp rune but used in Cmd, and in branches of BIG/KNT/PAT/etc)
    <   dump (only in commands: TODO maybe replace by multi-char rune)
    >   ----
    ?   LAM (named lambdas)
    ^   LET (non-recursive where clause, reversed multi-let)
    ~   REC (letrec)

-   TODO All command runes should be two-character execpt `=` and `\`.

    Use `/=`, `*=`, etc.  Having overlapping runes between commands and
    expressions is very confusing because commands are a superset of
    expressions (raw expressions interpreted as PRINT).

-   TODO Give `XDUMPY` a two-char rune.
-   TODO Give `XALIAS`, `/` a two-char rune.
-   TODO Use `/` and `\` both for type declarations (unparsed, unchecked).
-   TODO `$` for identifiery disambig.

    Use `$` instead of `/` for identifyer disambig. `$1` a$2`

-}

--  TODO Don't export expRex.
--
--  It's Currently needed for nice output on assertions, and no obvious
--  way to get our hands on the intermediate Val/Rul so that we can print
--  that instead.
--
--  This should be resolvable, and will allow us to delete much code.
module Sire.Syntax
    ( tagText
    , tagNam
    , tagStr
    , tagRex
    , lawNameText
    , readCmd
    , rexCmd
    , valRex
    , isNameChar
    , readTag
    , readSymb
    , idnTag
    , nameRex
    , textRex
    , parens
    , xtagApp
    , bodRexWid
    , bodRex
    , expRex
    , plunderMacro
    , MacroEnv(..)
    , joinRex
    )
where

import PlunderPrelude
import Data.ByteString.Base58
import Rex
import Sire.Types
import Sire.Backend

import Data.ByteString.Builder (byteStringHex, toLazyByteString)
import Data.Char               (isAlphaNum, isPrint)
import Data.Text.Encoding      (decodeUtf8')
import Plun.Types              (lawNameText)

import qualified Data.Char              as C
import qualified Data.Map               as M
import qualified Data.Text              as T

-- Definitions -----------------------------------------------------------------

tagText :: Tag -> Text
tagText = \case
  TAG "" n -> lawNameText n
  TAG a  n -> lawNameText n <> "/" <> a

tagStr :: Tag -> String
tagStr = unpack . tagText

-- Printing --------------------------------------------------------------------

tagRex :: Tag -> GRex v
tagRex (TAG i (LN n)) =
    if (i == natUtf8Exn n)
    then nameRex i
    else let lef = if okTagIdn i && False
                   then T BARE_WORD i
                   else T BARE_WORD (tshow n)
         in N SHUT_INFIX "^" [nameRex i, lef Nothing] Nothing
             -- TODO Consider using cords in some situations?

okTagIdn :: Text -> Bool
okTagIdn txt =
    fromMaybe False $ do
        guard (not $ null txt)
        guard (okTagIdnFragment txt)
        pure True
  where
    okTagIdnFragment :: Text -> Bool
    okTagIdnFragment "" = False
    okTagIdnFragment tx = all okTagIdnChar tx

    okTagIdnChar :: Char -> Bool
    okTagIdnChar '_' = True
    okTagIdnChar c   = C.isAlpha c

okIdn :: Text -> Bool
okIdn txt =
    fromMaybe False $ do
        guard (not $ null txt)
        (c, _) <- T.uncons txt
        guard (not $ C.isDigit c)
        pure (all okIdnChar txt)
  where
    okIdnChar '_' = True
    okIdnChar c   = C.isAlphaNum c

nameRex :: Text -> GRex v
nameRex = textRex BARE_WORD

textRex :: TextShape -> Text -> GRex v
textRex s t = T s t Nothing

barRex :: ByteString -> GRex v
barRex bs =
    case (decodeUtf8' bs) of
      Right t | okCord t -> T BARE_WORD "B"  $ Just $ T THIN_CORD t  Nothing
      _                  -> T BARE_WORD "BX" $ Just $ T THIN_CORD tx Nothing
        where tx = toStrict $ decodeUtf8 $ toLazyByteString $ byteStringHex bs
  where
    okCord t = all C.isPrint t && all okSpace t
    okSpace ' ' = True
    okSpace c   = not (C.isSpace c)



natRex :: Nat -> GRex v
natRex n = case natUtf8 n of
  Left _  -> nameRex (tshow n)
  Right s ->
    if | n < 256                        -> nameRex (tshow n)
       | all isNameChar s && length s>1 -> wrapCol $ nameRex s
       | isLineStr s                    -> T THIN_LINE s Nothing
       | isBlocStr s                    -> toLine s
       | otherwise                      -> nameRex (tshow n)
 where
  wrapCol r   = N SHUT_PREFIX "%" [r] Nothing

  toLine :: Text -> GRex v
  toLine = go . T.lines
    where
      go []     = T THIN_LINE "" $ Nothing
      go [l]    = T THIN_LINE l  $ Nothing
      go (l:ls) = T THIN_LINE l  $ Just (go ls)

  isOkPrint '\n' = False
  isOkPrint '\t' = False
  isOkPrint c    = isPrint c

  isLineStr = all isOkPrint . unpack

  isBlocStr s = and [ not (T.null s)
                    , T.last s == '\n'
                    , all isLineStr (T.lines s)
                    ]

xtagHasIdn :: XTag -> Bool
xtagHasIdn (XTAG "" Nothing _) = False
xtagHasIdn _                   = True

simpleBody :: XBod -> Bool
simpleBody XVAR{}         = True
simpleBody (XBAD XVNAT{}) = True
simpleBody (XBAD XVCOW{}) = True
simpleBody _              = False


-- TODO Explicitly check if recursive somehow.
lawRexWid :: XLaw -> GRex Rex
lawRexWid (XLAW t r b) =
    let mode = if simpleBody b && length r < 2 then SHUT_INFIX else NEST_INFIX
    in
    if xtagHasIdn t
    then N NEST_INFIX "?" [sigRex t r, bodRexWid b] NONE
    else N mode "&" [args (nameRex<$>r), bodRexWid b] NONE
  where
    args [x] = x
    args xs  = parens xs

lawRex :: XLaw -> GRex Rex
lawRex (XLAW t r b) =
    if xtagHasIdn t
    then N OPEN "?" [sigRex t r] (Just $ bodRex b)
    else N OPEN "&" [parens (nameRex<$>r)] (Just $ bodRex b)

valRexWid :: XVal -> GRex Rex
valRexWid = \case
  XVREF r   -> nameRex r
  XVNAT a   -> natRex a
  XVAPP f x -> parens (valRexWid <$> unCell [x] f)
  XVLAW l   -> lawRexWid l
  XVBAR b   -> barRex b
  XVROW r   -> rowRexWid r
  XVCOW n   -> nameRex ("R" <> tshow n)
  XVTAB t   -> tabRexWid t
  XVCAB k   -> N SHUT_PREFIX "%" [kys] NONE
                 where kys = N NEST_PREFIX "," (keyRex <$> toList k) NONE

tabRexWid :: Map Nat XVal -> GRex Rex
tabRexWid tab =
    N SHUT_PREFIX "%" [N NEST_PREFIX "," kvs NONE] NONE
  where
    kvs = mapToList tab <&> \(k,v) -> N SHUT_INFIX "=" [keyRex k, valRex v] NONE

keyRex :: Nat -> GRex v
keyRex nat =
    case natUtf8 nat of
        Right n | okIdn n -> nameRex n
        _                 -> natRex nat

rowRexWid :: Vector XVal -> GRex Rex
rowRexWid r = N NEST_PREFIX "," (valRexWid <$> toList r) Nothing

xvalXBod :: XVal -> XBod
xvalXBod = \case
  XVREF r   -> XVAR r
  XVNAT a   -> XBAD (XVNAT a)
  XVAPP f x -> XAPP (xvalXBod f) (xvalXBod x)
  XVLAW l   -> XBAD (XVLAW l)
  XVBAR b   -> XBAD (XVBAR b)
  XVROW b   -> XBAD (XVROW b)
  XVCOW n   -> XBAD (XVCOW n)
  XVTAB t   -> XBAD (XVTAB t)
  XVCAB t   -> XBAD (XVCAB t)

unCell :: [XVal] -> XVal -> [XVal]
unCell acc = \case
  XVAPP f x -> unCell (x:acc) f
  x         -> (x:acc)

parens :: [GRex v] -> GRex v
parens rexz = N NEST_PREFIX "|" rexz Nothing

wideApp :: [GRex v] -> GRex v
wideApp rexz =
    if all simple rexz
    then N SHUT_INFIX  "-" rexz Nothing
    else N NEST_PREFIX "|" rexz Nothing
  where
    simple (T BARE_WORD _ Nothing) = True
    simple _                       = False

valRex :: XVal -> GRex Rex
valRex = bodRex . xvalXBod


{-

TODO

    We should let the physical print system choose between these.

    Maybe a concept of a PRINTREF which has a wide and tall output.

    Wide output is used if it fits within within an 55-character output.

    That way, the depth can also be considered for such things.

    Also, the line-width can be tracked directly instead of constantly
    re-serializing subtrees to compute.

    Maybe something like this could work?

    data Box = { bWid :: (Rex, Int) :: Tree and character-count
               , bTal :: (Rex, Int) :: Tree and line-count
               }

-}

bodRex :: XBod -> GRex Rex
bodRex x@XLET{} = bodRexOpen x
bodRex x =
  if length wideOut <= 55
  then wideRex
  else bodRexOpen x
 where
  wideRex = bodRexWid x
  wideOut = rexLine (joinRex wideRex)

bodRexCont :: XBod -> GRex Rex
bodRexCont = forceOpen . bodRex

pattern NONE :: Maybe a
pattern NONE = Nothing

expRexWid :: XExp Rex -> GRex Rex
expRexWid = \case
    XEBED b              -> C b Nothing
    XEREF r              -> nameRex r
    XENAT a              -> natRex a
    XEBAR a              -> barRex a
    XEHAZ h              -> nameRex(decodeUtf8 $ encodeBase58 bitcoinAlphabet h)
    XEAPP f x            -> parens (go <$> unEApp [x] f)
    XEREC n v k          -> N NEST_INFIX "~" [nameRex n, go v] (Just $ go k)
    XELET n v k          -> N SHUT_INFIX "@" [nameRex n, go v] (Just $ go k)
    XEVEC vs             -> N NEST_PREFIX "," (go <$> vs) NONE
    XECOW n              -> nameRex ("R" <> tshow n)
    XETAB ps             -> N NEST_PREFIX "," (goPair <$> M.toAscList ps) NONE
    XECAB k              -> valRexWid (XVCAB k)
    XECOR _ b []         -> go b
    XECOR n b (f:fs)     -> N NEST_PREFIX "#"  [nameRex n, go b, goCore f fs] NONE
    XEBAT v x b          -> N NEST_PREFIX "**" [ go $ XETAB $ XEREF<$>v
                                               , go x
                                               , go b
                                               ] NONE
    XELAM (XFUN tg r b)  -> case tg of
                                XTAG "" NONE NONE ->
                                    nInf "&" [ goArgs r, go b ]
                                _ ->
                                    nInf "?"  [ sigRex tg r, go b ]

    XELIN (XFUN tg r b)  -> case tg of
                                XTAG "" NONE NONE ->
                                    nInf "!" [ goArgs r, go b ]
                                _ ->
                                    error "impossible: recursive inline lambda"
    XEPAT x f p          -> nPreC "?+" [go x,go f] (goPats $ mapToList p)
  where
    nInf r c    = N NEST_INFIX  r c NONE
    nPre r c    = N NEST_PREFIX r c NONE
    nPreC r c k = N NEST_PREFIX r c k

    go = expRexWid

    goPats []              = Nothing
    goPats ((n,(rs,x)):ps) = Just $ N NEST_PREFIX "++"
                                      [ nPre "," (natRex n : fmap nameRex rs)
                                      , go x
                                      ] (goPats ps)

    goArgs [s] = nameRex s
    goArgs ss  = nPre "|" (nameRex <$> ss)

    goCore (XFUN n r b) []     = N NEST_INFIX "++" [sigRex n r, go b] NONE
    goCore (XFUN n r b) (x:xs) = N NEST_INFIX "++" [sigRex n r, go b] (Just $ goCore x xs)

    goPair (k,v) = N SHUT_INFIX "=" [natRex k, go v] NONE

bodRexOpen :: XBod -> GRex Rex
bodRexOpen = \case
  XBAD (XVREF r)    -> nameRex r
  XBAD (XVNAT a)    -> natRex a
  XBAD (XVAPP f x)  -> colSeq (valRex <$> unCell [x] f)
  XCNS v            -> N OPEN "!" [valRex v] Nothing
  XAPP f x          -> niceApp (bodRex <$> unXApp [x] f)
  XLET n v k        -> N OPEN "@" [nameRex n, bodRex v] (Just $ bodRexCont k)
  XBAD (XVLAW l)    -> lawRex l
  XBAD (XVROW r)    -> comSeq (valRex <$> toList r)
  XBAD (XVCAB k)    -> N OPEN "%" [comSeq $ fmap keyRex $ toList k] NONE
  XBAD (XVTAB t)    -> N OPEN "%" [comSeq $ fmap f $ mapToList t] NONE
                         where
                           f :: (Nat, XVal) -> GRex Rex
                           f (k, v) = N OPEN "=" [keyRex k, valRex v] NONE
  x@(XVAR{})        -> bodRexWid x
  x@(XBAD(XVBAR{})) -> bodRexWid x
  x@(XBAD(XVCOW{})) -> bodRexWid x

colSeq :: [GRex v] -> GRex v
colSeq []     = error "impossible"
colSeq [x]    = N OPEN ":" [x] Nothing
colSeq (x:xs) = N OPEN ":" [x] (Just $ colSeq xs)

comSeq :: [GRex v] -> GRex v
comSeq []     = error "impossible"
comSeq [x]    = N OPEN "," [x] Nothing
comSeq (x:xs) = N OPEN "," [x] (Just $ comSeq xs)

expRex :: XExp Rex -> GRex Rex
expRex = \case
    XEBED v              -> C v Nothing
    XEREF r              -> nameRex r
    XENAT a              -> natRex a
    XEBAR a              -> barRex a
    XEAPP f x            -> niceApp (go <$> unEApp [x] f)
    XEHAZ h              -> expRexWid (XEHAZ h)
    XEREC n v k          -> N OPEN "@" [nameRex n, go v] (Just $ go k)
    XELET n v k          -> N OPEN "@" [nameRex n, go v] (Just $ go k)
    XEVEC vs             -> N OPEN "," (go <$> vs) Nothing
    XECOW n              -> nameRex ("R" <> tshow n)
    XETAB ps             -> N OPEN "," (goPair <$> M.toAscList ps) Nothing
    XECAB k              -> valRex (XVCAB k)
    XECOR _ b []         -> go b
    XECOR n b (f:fs)     -> N OPEN "#"  [nameRex n, go b] (Just $ goCore f fs)
    XEBAT v x b          -> N OPEN "**" [ go $ XETAB $ XEREF<$>v
                                        , go x
                                        ] (Just $ go b)
    XEPAT x f ps         -> N OPEN "?+" [go x, go f] (goPats $ mapToList ps)

    XELAM (XFUN tg r b)  ->
        case tg of
            XTAG "" NONE NONE -> nOpnC "&" [ goArgs r ]   (go b)
            _                 -> nOpnC "?" [ sigRex tg r] (go b)

    XELIN (XFUN tg r b)  ->
        case tg of
            XTAG "" NONE NONE -> nOpnC "!" [ goArgs r ]   (go b)
            _                 -> error "impossible: recursive inline lambda"

  where
    nOpnC r c k = N OPEN r c (Just k)
    nPre  r c   = N NEST_PREFIX r c NONE

    goPats []              = Nothing
    goPats ((n,(rs,x)):ps) = Just $ N OPEN "++"
                                      [ nPre "," (natRex n : fmap nameRex rs)
                                      , go x
                                      ] (goPats ps)

    goArgs [s] = nameRex s
    goArgs ss  = nPre "|" (nameRex <$> ss)

    goCore (XFUN n r b) []     = N NEST_INFIX "++" [sigRex n r, go b] NONE
    goCore (XFUN n r b) (x:xs) = N NEST_INFIX "++" [sigRex n r, go b] (Just $ goCore x xs)

    go = expRex

    goPair (n,x) = N OPEN "=" [natRex n, expRex x] Nothing

sigRex :: XTag -> [Text] -> GRex v
sigRex n rs = N NEST_PREFIX "|" (xtagRex n : fmap nameRex rs) Nothing

forceOpen :: GRex v -> GRex v
-- ceOpen (T s t k)        = T s t (forceOpen <$> k)
forceOpen x@(N OPEN _ _ _) = x
forceOpen (N _ "|" xs k)   = N OPEN "|" xs k
forceOpen other            = N OPEN "|" [other] NONE
-- ceOpen (C c k)          = C c (forceOpen <$> k)

joinRex :: GRex (GRex v) -> GRex v
joinRex = \case
    T s t k      -> T s t (joinRex <$> k)
    C c Nothing  -> c
    C c (Just k) -> rexAddCont c (joinRex k)
    N m r x k    -> N m r (joinRex <$> x) (joinRex <$> k)

rexAddCont :: GRex v -> GRex v -> GRex v
rexAddCont (T s t Nothing) c    = T s t (Just c)
rexAddCont (T s t (Just k)) c   = T s t (Just $ rexAddCont k c)
rexAddCont (N m r x Nothing) c  = N m r x (Just c)
rexAddCont (N m r x (Just k)) c = N m r x (Just $ rexAddCont k c)
rexAddCont (C x Nothing) c      = C x (Just c)
rexAddCont (C x (Just k)) c     = C x (Just $ rexAddCont k c)

niceApp :: [GRex Rex] -> GRex Rex
niceApp rs =
  let normalRx = N OPEN "|" rs Nothing
      outFile  = rexFile (joinRex normalRx)
      outWidth = length outFile
      outLines = length $ lines $ outFile
  in
      if outWidth <= 45 && outLines <= 1
      then normalRx
      else case reverse rs of
             k:l:sx -> N OPEN "|" (reverse (l:sx)) (Just $ forceOpen k)
             []     -> N OPEN "|" [] Nothing
             [x]    -> N OPEN "|" [x] Nothing

unXApp :: [XBod] -> XBod -> [XBod]
unXApp acc = \case
    XAPP f x -> unXApp (x:acc) f
    x        -> (x:acc)

eapp :: (XExp v, [XExp v]) -> XExp v
eapp (f,[])   = f
eapp (f,x:xs) = eapp (XEAPP f x, xs)

unEApp :: [XExp v] -> XExp v -> [XExp v]
unEApp acc = \case
    XEAPP f x -> unEApp (x:acc) f
    x         -> (x:acc)

bodRexWid :: XBod -> GRex Rex
bodRexWid = \case
    XVAR v           -> nameRex v
    XBAD (XVREF r)   -> nameRex r
    XBAD (XVNAT a)   -> natRex a
    XBAD (XVAPP f x) -> N SHUT_INFIX ":" (valRexWid <$> unCell [x] f) NONE
    XBAD (XVBAR b)   -> barRex b
    XBAD (XVROW b)   -> valRexWid $ XVROW b
    XBAD (XVCOW n)   -> nameRex ("R" <> tshow n)
    XBAD (XVTAB t)   -> valRexWid (XVTAB t)
    XBAD (XVCAB k)   -> valRexWid (XVCAB k)
    XBAD (XVLAW l)   -> lawRexWid l
    XCNS v           -> N SHUT_PREFIX "!" [valRexWid v] NONE
    XAPP f x         -> wideApp (bodRexWid <$> unXApp [x] f)
    XLET n v k       -> N NEST_INFIX "@" [nameRex n, bodRexWid v]
                                         (Just $ bodRexWid k)

xtagApp :: XTag -> [Text] -> GRex v
xtagApp t [] = xtagRex t
xtagApp t as = parens (xtagRex t : fmap nameRex as)

xtagRex :: XTag -> GRex v
xtagRex = \case
    XTAG n d Nothing       -> disaRex n d
    XTAG n d (Just (LN t)) -> N SHUT_INFIX "^" [disaRex n d, natRex t] NONE
  where
    disaRex ""  Nothing  = nameRex "_"
    disaRex n   Nothing  = nameRex n
    disaRex ""  (Just d) = N SHUT_INFIX "/" [nameRex "_", nameRex d] NONE
    disaRex n   (Just d) = N SHUT_INFIX "/" [nameRex n, nameRex d] NONE


-- Parsing ---------------------------------------------------------------------

rexCmd :: ∀m. MacroEnv m Pln -> Rex -> Either Text (XCmd Pln)
rexCmd env rex =
    preProcess rex >>= load
 where
  preProcess :: Rex -> Either Text (GRex v)
  preProcess = fmap rewriteLeafJuxtaposition
             . applyInfixPrec

  load :: GRex Pln -> Either Text (XCmd Pln)
  load = resultEitherText dropEmbed rex
       . runReading env readCmd
       . stripComments

  dropEmbed :: Show v => GRex v -> Rex
  dropEmbed = \case
      T s t k     -> T s t                    (dropEmbed <$> k)
      C c k       -> T THIN_CORD (tshow c)    (dropEmbed <$> k)
      N m r xs mK -> N m r (dropEmbed <$> xs) (dropEmbed <$> mK)

rewriteLeafJuxtaposition :: GRex a -> GRex a
rewriteLeafJuxtaposition = go
  where
    go = \case
      T s t Nothing  -> T s t Nothing
      T s t (Just x) -> N SHUT_INFIX "-" [T s t Nothing, go x] Nothing
      N m r p k      -> N m r (go <$> p) (go <$> k)
      C x k          -> C x (go <$> k)

runReading :: MacroEnv m v -> Red m v a -> GRex v -> Result v a
runReading env (READT act) inp
    = unsafePerformIO do
        flip runReaderT env
            $ runResultT
            $ act inp

runePrec :: Text -> Int
runePrec "-"  = 9
runePrec ":=" = 8
runePrec "="  = 8
runePrec _    = 1

applyInfixPrec :: Rex -> Either Text (GRex a)
applyInfixPrec = go
  where
    go :: ∀a. Rex -> Either Text (GRex a)
    go = \case
        N m r c k         -> N m r <$> traverse go c <*> traverse go k
        T s t k           -> T s t <$> traverse go k
        C(AS x []) mK     -> go (N SHUT_INFIX "|" [x] mK)
        C(AN x []) mK     -> go (N NEST_INFIX "|" [x] mK)
        C(AS x (r:rs)) mK -> do (run, cs) <- disamb x (r :| rs)
                                k <- traverse go mK
                                go (N SHUT_INFIX run cs k)
        C(AN x (r:rs)) mK -> do (run, cs) <- disamb x (r :| rs)
                                k <- traverse go mK
                                go (N NEST_INFIX run cs k)

    mak h [] = h
    mak h t  = C (AS h $ reverse t) Nothing

    splitIt :: Text -> (Rex, [(Text, Rex)]) -> [(Text, Rex)] -> [Rex]
    splitIt _ (h,t) []                   = mak h t : []
    splitIt r (h,t) ((r2,i2):rs) | r==r2 = mak h t : splitIt r (i2,[]) rs
    splitIt r (h,t) ((r2,i2):rs)         = splitIt r (h,(r2,i2):t) rs

    disamb :: Rex -> NonEmpty (Text, Rex) -> Either Text (Text, [Rex])
    disamb x ((r,i) :| mor) = do run <- lowestPrec r (fst <$> mor)
                                 pure $ (run,) $ splitIt run (x,[]) ((r,i):mor)

lowestPrec :: Text -> [Text] -> Either Text Text
lowestPrec o os = go (runePrec o, o) os
 where
  go :: (Int, Text) -> [Text] -> Either Text Text
  go (_, r) []              = Right r
  go (p, r) (r2:rs) | r==r2 = go (p,r) rs
  go (p, r) (r2:rs)         =
      let p2 = (runePrec r2) in
      case compare p p2 of
          EQ -> Left ( "Two runes have same precidence: " <> tshow (r,r2))
          LT -> go (p,r) rs
          GT -> go (p2,r2) rs

stripComments :: GRex v -> GRex v
stripComments = \case
    top@(N _ "#~" _ Nothing) -> top
    N _ "#~" _ (Just k)      -> stripComments k
    top                      -> fromMaybe top (go top)
  where
    go :: GRex v -> Maybe (GRex v)
    go rex = case rex of
        T{}           -> Just rex
        N _ "#~" _ mK -> mK >>= go
        N m r c k     -> Just $ N m r (catMaybes $ fmap go c) (k >>= go)
        C{}           -> error "Impossibe: ambiguous infix already resolved"

resultEitherText :: (GRex v -> Rex) -> Rex -> Result v a -> Either Text a
resultEitherText cvt blk res =
  case resultEither res of
    Right x       -> pure x
    Left (mR,err) -> Left (errMsg <> maybe "" showForm mR <> showBlock)
     where
      errMsg      = unlines [ "== READER ERROR ==", "", err ]
      showForm rx = unlines [ "In Form:" , "" , dent "  " (rexFile $ cvt rx) ]
      showBlock   = unlines [ "In Block:" , "" , dent "  " (rexFile blk) ]

dent :: Text -> Text -> Text
dent pre = unlines . fmap dentLine . lines
 where dentLine :: Text -> Text
       dentLine "" = pre
       dentLine ln = pre <> "  " <> ln

type Red m v = ReadT v (ReaderT (MacroEnv m v) IO)

readCmd :: ∀m. Red m Pln (XCmd Pln)
readCmd = asum
  [ do rune "!"
       checks <- slip1N "!" readExpr readExpr
       pure $ XCHECK (checks <&> \(v,vs) -> foldl' XEAPP v vs)
  , do rForm2c "#=" readCord readExpr XMACRO
  , do rForm1c "#?" readExpr XPLODE
  , do rune "<"
       (v,vs) <- form1Nc readExpr readExpr
       pure (XDUMPY $ foldl' XEAPP v vs)
  , rForm2c  "*" readNamz readExpr XVOPEN
  , do rune ":="
       ((t,a,as), b) <- form2c (do rune "|" <|> rune "-"
                                   form2N readXTag readSymb readSymb)
                             readBod
       pure (XMKRUL (XLAW t (a:as) b))
  , do rune "/"
       res <- slip2 "/" readSymb readVal
       pure (XALIAS res)
  , do rune "="
       res <- slip2 "=" readSigy readExpr
       pure $ XDEFUN (res <&> \((t,args),b) -> XFUN t args b)
  , do rune "<-"
       let readIdns = rune "," >> form2 readSymb readSymb
       ((rid,res), exr) <- form2 readIdns readExpr
       pure (XIOEFF rid res exr)
  , XSAVEV <$> (rune "<<" >> form1 readExpr)
  , XPRINT <$> readExpr
  ]

readArgs :: Red m v (Text, [Text])
readArgs = asum
    [ readSymb <&> \i -> (i, [])
    , do rune "|" <|> rune "-"
         form1N readSymb readSymb
    ]

readSigy :: Red m v (XTag, [Text])
readSigy = asum
    [ readSymb <&> \i -> (idnXTag i, [])
    , do rune "|" <|> rune "-"
         form1N readXTag readSymb
    ]

readVal :: Red m v XVal
readVal = asum [ XVNAT <$> readNat
                , XVREF <$> readSymb
                , do rune "|" <|> rune "-"
                     (x, xs) <- form1Nc readVal readVal
                     let cell a []    = a
                         cell a (b:c) = cell (XVAPP a b) c
                     pure (cell x xs)
                , do rune ","
                     vs <- formN readVal
                     let r = fromIntegral (length vs + 1) :: Nat
                     let vn = XVNAT 1 `XVAPP` XVNAT r
                                      `XVAPP` XVNAT 1
                                      `XVAPP` XVNAT 0
                     let xc f []     = f
                         xc f (x:xs) = xc (XVAPP f x) xs
                     pure (xc vn vs)
                ]

readCow :: Red m v Nat
readCow = matchLeaf "R[0-9]+" \case
    (BARE_WORD, "R0") -> Just 0

    (BARE_WORD, (unpack -> ('R' : n : ns))) -> do
        guard (all C.isDigit (n:ns))
        guard (n /= '0')
        readMay (n:ns)
    _ -> Nothing

readBod :: Red m v XBod
readBod = asum [ XCNS . XVNAT <$> readNat -- TODO: Depends on scope.
               , readCow <&> \case 0 -> XBAD (XVROW mempty)
                                   n -> XBAD (XVCOW n)
               , XVAR <$> readSymb
               , rune "|" >> (uncurry xapp <$> form1Nc readBod readBod)
               , rune "-" >> (uncurry xapp <$> form1Nc readBod readBod)
               , rune ":" >> form1c (XBAD <$> readVal)
                             -- TODO Accept multiple params
               , rune "!" >> form1c (XCNS <$> readVal)
               , rune "@" >> do (n, v, k) <- form3c readSymb readBod readBod
                                pure (XLET n v k)
               ]

xapp :: XBod -> [XBod] -> XBod
xapp a []    = a
xapp a (b:c) = xapp (XAPP a b) c

readExcplicitTag :: Red m v LawName
readExcplicitTag =
  fmap LN $
  asum [ utf8Nat <$> readCord
       , utf8Nat <$> readSymb
       , readNat
       ]

readTag :: Red m v Tag
readTag = asum
    [ idnTag <$> readSymb
    , do rune "^"
         (i,t) <- form2 readSymb readExcplicitTag
         pure (TAG i t)
    ]

readXTag :: Red m v XTag
readXTag = asum
    [ idnXTag <$> readSymb
    , rForm2 "^" readSymb readNat \n t -> XTAG n NONE (Just (LN t))
    ]

idnTag :: Text -> Tag
idnTag t = TAG t (LN $ utf8Nat t)

readSymb :: Red m v Text
readSymb = matchLeaf "symbol" \case
    (BARE_WORD,n) | okIdn n -> Just n
    _                       -> Nothing

readSymbEq :: Text -> Red m v ()
readSymbEq eq = matchLeaf eq \case
    (BARE_WORD,n) | n==eq -> Just ()
    _                     -> Nothing

readHash :: Red m v Word256
readHash =
    matchLeaf "hash" \(s,n) -> do
        guard (s == BARE_WORD)
        guard (length n > 40)
        bs <- decodeBase58 bitcoinAlphabet (encodeUtf8 n)
        guard (length bs == 32)
        pure bs

readName :: Red m v Text
readName = matchLeaf "name" \case
    (BARE_WORD,n) -> Just n
    _             -> Nothing

readCord :: Red m v Text
readCord = matchLeaf "cord" \case
    (THIN_CORD,t) -> Just t
    (THIC_CORD,t) -> Just t
    _             -> Nothing

readLine :: Red m v Text
readLine = matchLeaf "page" \case
    (THIC_LINE,l) -> Just l
    (THIN_LINE,l) -> Just l
    _             -> Nothing

isNameChar :: Char -> Bool
isNameChar '_' = True
isNameChar c   = isAlphaNum c

readNat :: Red m v Nat
readNat = readNumb <|> readColn <|> readTape <|> line
  where
    line = utf8Nat <$> readLine
    readTape = utf8Nat <$> readCord
    readColn = rune "%" >> form1 (utf8Nat <$> readName)

readNumb :: Red m v Nat
readNumb = matchName "nat" \n ->
             case unpack n of
               '_':_ -> Nothing
               _     -> readMay (filter (/= '_') n)

-- Functions -------------------------------------------------------------------

data MacroEnv m v = MacroEnv
    { meGenSymState :: IORef Nat
    , meGlobalScope :: v
    , meMacros      :: Map Text v
    }

readNamz :: Red m v [Text]
readNamz = asum
    [ readSymb <&> singleton
    , rune "," >> formN readSymb
    ]

readCnNm :: Red m v Text
readCnNm = rune "%" >> form1 readName

type MacroExpander v
    = v
    -> Nat
    -> [(GRex v)]
    -> Maybe (GRex v)
    -> IO (Either Text (Nat, (GRex v)))

runMacro :: MacroExpander Pln -> Red m Pln (XExp Pln)
runMacro macro = do
    (xs, mK) <- readNode
    env@MacroEnv{..} <- lift ask
    n <- readIORef meGenSymState
    liftIO (macro meGlobalScope n xs mK) >>= \case
        Left err -> throwError err
        Right (!used, !res) -> do
            modifyIORef' meGenSymState (+used)
            liftIO ( flip runReaderT env
                   $ runResultT
                   $ runReadT readExpr res
                   ) >>= readResult

readExpr :: Red m Pln (XExp Pln)
readExpr = do
    MacroEnv{meMacros} <- lift ask
    let macros = mapToList meMacros <&> \(r,h) -> do rune r
                                                     runMacro (plunderMacro h)
    asum (fixed <> macros)
  where
    fixed =
        [ XEBED <$> readExtra
        , XEHAZ <$> readHash
        , do rune "%"
             form1 $ asum
                 [ XENAT . utf8Nat <$> readName
                 , either XECAB XETAB <$> readTab readExpr XEREF
                 ]
        , XENAT <$> readNat
        , readCow <&> \case 0 -> XEVEC mempty
                            n -> XECOW n
        , XEREF <$> readSymb
        , rFormN1c "."  readExpr readExpr          appTo
        , rForm3c  "~"  readSymb readExpr readExpr XEREC
        , rForm3c  "**" readTSig readExpr readExpr XEBAT
        , rForm2c  "?"  readSigy readExpr          nameLam
        , rForm2c  "&"  readArgs readExpr          anonLam
        , rForm2c  "!"  readArgs readExpr          anonLin
        , rFormN1c "^"  readExpr readBind          mkWhere
        , rForm3c  "#"  readCnNm readExpr readCore XECOR
        , rForm2c  "+"  readExpr readPats          mkPat
        , rForm1Nc "|"  readExpr readExpr          apple
        , rForm1Nc "-"  readExpr readExpr          apple
        , rForm3c  "@"  readSymb readExpr readExpr XELET
        , do rune ","
             asum [ XEVEC <$> formN readExpr
                  , do sequ <- slip1N "," readExpr readExpr
                       pure $ XEVEC $ fmap eapp sequ
                  ]
        ]

    dropFallback :: Ord k => Map (Maybe k) v -> Map k v
    dropFallback = mapFromList . mapMaybe f . mapToList
      where f (Nothing, _) = Nothing
            f (Just k, v)  = Just (k, v)

    mkPat :: XExp v -> Map (Maybe Nat) ([Text], XExp v) -> XExp v
    mkPat x p = XEPAT x fb (dropFallback p)
      where fb = fromMaybe (XENAT 0) (snd <$> lookup Nothing p)

    apple f []    = f
    apple f (b:c) = apple (XEAPP f b) c

    appTo xs f = apple f xs

    mkWhere :: [XExp v] -> [(Text, XExp v)] -> XExp v
    mkWhere []     []         = XENAT 0 -- TODO: Implement and use `form1N1c`
    mkWhere (f:xs) []         = apple f xs
    mkWhere fxs    ((n,v):bs) = XELET n v (mkWhere fxs bs)

    anonLam (r,rs) x = XELAM (XFUN (idnXTag "") (r:rs) x)
    nameLam (t,rs) x = XELAM (XFUN t            rs     x)
    anonLin (r,rs) x = XELIN (XFUN (idnXTag "") (r:rs) x)

    readPats :: Red m Pln (Map (Maybe Nat) ([Text], XExp Pln))
    readPats = do
        rune "="
        let readPatr = fmap (,[]) readNatCab
                    <|> rForm1N "," (Just<$>readNat) readSymb (,)
        ps <- slip2 "=" readPatr readExpr
        pure $ mapFromList (ps <&> \((n,rs),x) -> (n,(rs,x)))

    readNatCab :: Red m Pln (Maybe Nat)
    readNatCab = fmap Just readNat <|> (readSymbEq "_" $> Nothing)

    readTSig :: Red m Pln (Map Nat Text)
    readTSig =
        fmap mapFromList
        $ asum
        [ rune "," >> formN sigElem
        , singleton <$> sigElem
        ]
      where
        sigElem = do
             rune "="
             asum
                 [ form1 readKey <&> \k -> (k, natUtf8Exn k)
                 , form2 readKey readSymb
                 ]

    readBind = rune "=" >> slip2 "=" readSymb readExpr

    readCore = do
        rune "="
        res <- slip2 "=" readSigy readExpr
        pure (res <&> \((t,rs),b) -> XFUN t rs b)

rForm1c :: Text -> Red m v a -> (a -> b) -> Red m v b
rForm1c r x k = rune r >> form1c x >>= \a -> pure (k a)

rFormN1c :: Text -> Red m v a -> Red m v b -> ([a] -> b -> c) -> Red m v c
rFormN1c r x y k = rune r >> formN1c x y >>= \(a,b) -> pure (k a b)

rForm3c :: Text -> Red m v a -> Red m v b -> Red m v c -> (a -> b -> c -> d)
        -> Red m v d
rForm3c r x y z k = rune r >> form3c x y z >>= \(a,b,c) -> pure (k a b c)

rForm2 :: Text -> Red m v a -> Red m v b -> (a -> b -> c) -> Red m v c
rForm2 r x y k = rune r >> form2 x y >>= \(a,b) -> pure (k a b)

rForm2c :: Text -> Red m v a -> Red m v b -> (a -> b -> c) -> Red m v c
rForm2c r x y k = rune r >> form2c x y >>= \(a,b) -> pure (k a b)

rForm1Nc :: Text -> Red m v a -> Red m v b -> (a -> [b] -> c) -> Red m v c
rForm1Nc r x y k = rune r >> form1Nc x y >>= \(a,bs) -> pure (k a bs)

rForm1N :: Text -> Red m v a -> Red m v b -> (a -> [b] -> c) -> Red m v c
rForm1N r x y k = rune r >> form1N  x y >>= \(a,bs) -> pure (k a bs)

idnXTag :: Text -> XTag
idnXTag n = XTAG n NONE NONE

-- TODO Support tall form
readTab
    :: ∀a m v
     . Red m v a
    -> (Text -> a)
    -> Red m v (Either (Set Nat) (Map Nat a))
readTab ele var = do
    rune ","

    let keyEle n = (utf8Nat n, var n)

    res :: [Either Nat (Nat,a)]
        <- formN $ asum
                 [ Left <$> readKey
                 , Right <$> (rune "=" >> (    form2 readKey ele
                                           <|> (keyEle <$> form1 readSymb)
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

readKey :: Red m v Nat
readKey = asum
    [ readNumb
    , utf8Nat <$> readSymb
    , utf8Nat <$> readCord
    ]


-- Macros ----------------------------------------------------------------------

plunderMacro :: Pln -> MacroExpander Pln
plunderMacro macroLaw macEnv nex xs mK = do
    let vs  = ROW $ fromList (rexVal <$> xs)
    let kv  = maybe (NAT 0) rexVal mK
    let res = valPlun (REF macroLaw `APP` REF macEnv
                                    `APP` NAT nex
                                    `APP` vs
                                    `APP` kv)

    pure $ case plunVal res of
        NAT (natUtf8 -> Right msg) ->
            Left msg
        ROW (toList -> [NAT used, body]) -> do
            rez <- loadRex valPlun body
            pure (used, rez)
        _  ->
            Left "Invalid Macro Expansion"
--where
--  showRaw :: v -> Text
--  showRaw x = "Δ" <> tshow (unsafeCoerce x :: Plun.Val)

loadRex :: (Val Pln -> Pln) -> Val Pln -> Either Text (GRex Pln)
loadRex putVal =
    loadRow >=> loadRexRow
  where
    recur = loadRex putVal

    loadRow (ROW r) = Right (toList r)
    loadRow vl      = bad "node" vl

    loadName (NAT (natUtf8 -> Right n)) = Right n
    loadName vl                         = bad "name" vl

    loadCord (NAT (natUtf8 -> Right n)) = Right n
    loadCord vl                         = bad "cord" vl

    bad :: Show w => Text -> w -> Either Text b
    bad expect val = Left $ concat [ "Bad macro expansion, not a "
                                   , expect
                                   , ": "
                                   , tshow val
                                   ]

    loadRexRow = \case
        [NAT 0, r, x] -> loadRexRow [NAT 0, r, x, NAT 0]
        [NAT 1, n]    -> loadRexRow [NAT 1, n, NAT 0]
        [NAT 2, c]    -> loadRexRow [NAT 2, c, NAT 0]
        [NAT 3, p]    -> loadRexRow [NAT 3, p, NAT 0]
        [NAT 4, z]    -> loadRexRow [NAT 4, z, NAT 0]

        [NAT 0, rv, xsv, mkv] -> do
            rwne <- loadCord rv
            xs <- toList <$> (loadRow xsv >>= traverse recur)
            mK <- loadCont mkv
            pure (N OPEN rwne xs mK)

        -- TODO Change this to match new representation
        [NAT 1, n, k] -> T BARE_WORD <$> loadName n <*> loadCont k
        [NAT 2, n, k] -> T THIN_CORD <$> loadCord n <*> loadCont k
        [NAT 3, p, k] -> T THIN_LINE <$> loadCord p <*> loadCont k
        [NAT 4, z, k] -> C (putVal z) <$> loadCont k
        (NAT n : _)   -> Left ("Invalid rex node (key=" <> tshow n <> ")")
        _             -> Left "Invalid rex node"

    loadCont (NAT 0) = pure Nothing
    loadCont mkv     = Just <$> recur mkv

rexVal :: GRex Pln -> Val Pln
rexVal = \case
    -- TODO Change this to match new representation:
    -- {0 m p k}
    -- {1 m t k}
    -- {2 v k}

    N  _ rn xs mK     -> rowV [ NAT 0
                              , cordV rn
                              , rowV (rexVal <$> xs)
                              , maybe (NAT 0) rexVal mK
                              ]
    T BARE_WORD n k   -> rowV [ NAT 1, cordV n, maybe (NAT 0) rexVal k ]
    T THIN_CORD c k   -> rowV [ NAT 2, cordV c, maybe (NAT 0) rexVal k ]
    T THIN_LINE p k   -> rowV [ NAT 3, cordV p, maybe (NAT 0) rexVal k ]
    C bed k           -> rowV [ NAT 4, REF bed, maybe (NAT 0) rexVal k ]

    T THIC_CORD c k   -> rexVal (T THIN_CORD c k) -- TODO
    T THIC_LINE p k   -> rexVal (T THIN_LINE p k)
  where
    cordV = NAT . utf8Nat

    rowV :: [Val v] -> Val v
    rowV = ROW . fromList
