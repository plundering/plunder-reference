{-# OPTIONS_GHC -Wall   #-}
{-# OPTIONS_GHC -Werror #-}

module Loot.Syntax
    ( resultEitherText
    , keyRex
    , readSigy
    , readArgs
    , readXTag
    , simpleTag
--  , tagText
    , tagNam
--  , tagStr
--  , tagRex
    , lawNameText
    , readCmd
    , rexCmd
    , valRex
    , isNameChar
    , readTag
    , readIdnt
    , readIdnTxt
    , readSymb
    , readBymb
--  , idnTag
    , symbRex
    , nameRex
    , textRex
    , parens
    , xtagApp
    , bodRex
    , joinRex
    , readKey
    , readName
    , readNumb
    , readNat
    , readCord
    , readCow
    , rForm1
    , rForm1c
    , rFormN1c
    , rForm3c
    , rForm2
    , rForm2c
    , rFormNc
    , rForm1Nc
    , bodBox
    , barBox
    , barRex
    , readOpenCab
    )
where

import Loot.Types
import PlunderPrelude
import Rex

import Data.ByteString.Builder (byteStringHex, toLazyByteString)
import Data.Char               (isAlphaNum, isPrint)
import Data.Text.Encoding      (decodeUtf8')
import Plun.Types              (lawNameText)

import qualified Data.ByteString.Base16 as B16
import qualified Data.Char              as C
import qualified Data.Text              as T


-- Types -----------------------------------------------------------------------

type Red v = ReadT v IO

data Box = BOX
    { boxWidth :: Int
    , boxWide  :: Rex
    , boxTall  :: GRex Box
    }


-- Pretty Printing -------------------------------------------------------------

leafBox :: Int -> GRex Void -> Box
leafBox wid rex = BOX wid rex (absurd<$>rex)

wideTextBox :: TextShape -> Text -> Box
wideTextBox s t =
    leafBox wid (T s t Nothing)
  where
    wid = length t + (if s==BARE_WORD then 0 else 2)

nameBox :: Text -> Box
nameBox = wideTextBox BARE_WORD

symbBox :: Symb -> Box
symbBox symb =
    case (symb, natUtf8 symb) of
        (0, _)                   -> buk (nameBox "0")
        (_, Right nm) | okIdn nm -> nameBox nm
        (_, Right nm) | okTxt nm -> buk (cordBox nm)
        _                        -> buk (nameBox (tshow symb))
  where
    buk :: Box -> Box
    buk b = leafBox (1 + boxWidth b)
                    (N SHUT_PREFIX "$" [boxWide b] Nothing)


textBox :: TextShape -> Text -> Box
textBox style t =
    case (style, wideTextBox style t) of
      (THIN_LINE, bax) -> bax { boxTall = T THIN_LINE t Nothing }
      (THIC_LINE, bax) -> bax { boxTall = T THIC_LINE t Nothing }
      (_,         bax) -> bax

pageBox :: TextShape -> [Text] -> Box
pageBox shape linez =
    leafBox wid (go linez)
  where
    wid = (2 * length linez) + sum (length<$>linez)

    go []     = T shape "" Nothing
    go [l]    = T shape l  Nothing
    go (l:ls) = T shape l  (Just $ go ls)

cordBox :: Text -> Box
cordBox txt =
    case lines txt of
        [] -> textBox THIC_CORD ""
        [l] | hasBot -> error "TODO: Escaping" l
        [l] | hasTic -> textBox THIC_CORD l
        [l] | hasQuo -> textBox THIN_CORD l
        [l]          -> textBox THIN_CORD l
        ls           -> pageBox THIC_LINE ls
  where
    hasBot = hasTic && hasQuo
    hasTic = elem '\'' txt
    hasQuo = elem '"'  txt

natBox :: Nat -> Box
natBox n =
  case natUtf8 n of
    Left _                                   -> nameBox (tshow n)
    Right s | n < 256                        -> nameBox (tshow n)
            | all isNameChar s && length s>1 -> cenBox s
            | isLineStr s                    -> textBox THIN_LINE s
            | isBlocStr s                    -> pageBox THIN_LINE (T.lines s)
            | otherwise                      -> nameBox (tshow n)
  where
    isOkPrint '\n' = False
    isOkPrint '\t' = False
    isOkPrint c    = isPrint c

    isLineStr = all isOkPrint . unpack

    isBlocStr s = and [ not (T.null s)
                      , T.last s == '\n'
                      , all isLineStr (T.lines s)
                      ]


-- XVAPP f x -> parens (valRexWid <$> unCell [x] f)
-- XVAPP f x -> niceApp (valRex <$> unXVApp [x] f)

box :: Box -> GRex Box
box b = C b NONE

forceBoxOpen :: Box -> Box
forceBoxOpen =
    \bax -> bax { boxWide = go (boxWide bax) }
  where
    go :: Rex -> Rex
    go (C joke _)       = absurd joke
    go x@(N OPEN _ _ _) = x
    go (N _    "|" p h) = N OPEN "|" p h
    go x                = N OPEN "|" [x] NONE

valBox :: XVal -> Box
valBox = \case
    XVREF r   -> symbBox r
    XVNAT a   -> natBox a
    XVAPP f x -> boxApply (valBox <$> unCell [x] f)
    XVLAW l   -> lawBox l
    XVBAR b   -> barBox b
    XVROW r   -> rowBox r
    XVCOW n   -> nameBox ("R" <> tshow n)
    XVTAB t   -> tabBox t
    XVCAB k   -> cabBox k

valBoxes :: XVal -> [Box]
valBoxes = \case
    XVAPP f x -> valBox <$> unCell [x] f
    v         -> [valBox v]


rowBox :: Vector XVal -> Box
rowBox row =
    BOX siz wid tal
  where
    tal = if null row then (absurd<$>wid) else
            openSeq ",," (fmap box . valBoxes <$> toList row)
    wid = N NEST_PREFIX "," (boxWide <$> kid) Nothing
    siz = 1 + length row + sum (boxWidth <$> kid)
    kid = valBox <$> toList row

tabBox :: Map Nat XVal -> Box
tabBox tab =
    BOX siz wid tal
  where
    siz = if null tab
          then 3
          else 2 + length keySizes + sum keySizes

    wid :: Rex
    wid = N SHUT_PREFIX "%" [N NEST_PREFIX "," wideKeys NONE] NONE

    tal :: GRex Box
    tal = openSeq "%%"
        $ turn (mapToList tab) \(k,v) ->
            let kr = N SHUT_PREFIX "=" [box(keyBox k)] Nothing
            in if keyIsValue k v
               then [kr]
               else kr : (box <$> valBoxes v)

    keyIsValue k (XVREF w) = (k == w)
    keyIsValue _ _         = False

    keySizes = fst <$> wideSizedKeys
    wideKeys = snd <$> wideSizedKeys

    wideSizedKeys :: [(Int, GRex Void)]
    wideSizedKeys = do
        (k, v) <- mapToList tab
        let kb = keyBox k
        let vb = valBox v
        pure $ if (XVREF k == v)
            then let wd = 1 + boxWidth kb
                 in (wd, N SHUT_PREFIX "=" (boxWide <$> [kb]) NONE)
            else let wd = 1 + boxWidth kb + boxWidth vb
                  in (wd, N SHUT_INFIX "=" (boxWide <$> [kb,vb]) NONE)


cabBox :: Set Nat -> Box
cabBox k =
    BOX width wid tal
  where
    tal = openSeq "%%" (singleton . box <$> keysBox)
    wid = N SHUT_PREFIX "%" [widKeys] NONE
    keysBox = keyBox <$> toList k
    widKeys = N NEST_PREFIX "," (boxWide <$> keysBox) NONE
    width = 2 + sum (boxWidth <$> keysBox)

barRex :: ByteString -> Rex
barRex = boxWide . barBox

barBox :: ByteString -> Box
barBox bs =
    BOX wid rex (absurd<$>rex)
  where
    okCord t = all C.isPrint t && all okSpace t
    okSpace ' ' = True
    okSpace c   = not (C.isSpace c)

    tx = toStrict $ decodeUtf8 $ toLazyByteString $ byteStringHex bs

    rex = N SHUT_INFIX "#" [ T BARE_WORD pre NONE
                           , post
                           ] NONE

    (wid, (pre, post)) = case (decodeUtf8' bs) of
        Right t | okName t -> ( 2 + length t
                              , ("b", T BARE_WORD t  NONE)
                              )

        Right t | okCord t -> ( 4 + length t
                              , ("b", T THIN_CORD t NONE)
                              )

        _                  -> ( 2 + length tx
                              , ("x", T BARE_WORD tx NONE)
                              )

boxRex :: Box -> Rex
boxRex BOX{..} =
    if boxWidth < 55
    then boxWide
    else joinRex (boxRex <$> boxTall)

valRex :: XVal -> GRex Rex
valRex = fmap absurd . boxRex . valBox

bodRex :: XBod -> GRex Rex
bodRex = fmap absurd . boxRex . bodBox

-- Printing --------------------------------------------------------------------

{-
tagRex :: Tag -> GRex v
tagRex (TAG i (LN n)) =
    if (i == n)
    then xtagRex (XTAG i NONE)
    else xtagRex (XTAG i (Just $ LN n))
-}

{-
okTagSymb :: Symb -> Bool
okTagSymb sym =
    let txt = error "TODO" sym in
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
-}

okName :: Text -> Bool
okName txt = not (null txt) && all isNameChar txt

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

textRex :: TextShape -> Text -> GRex v
textRex s t = T s t Nothing

nameRex :: Text -> GRex v
nameRex = textRex BARE_WORD

cenBox :: Text -> Box
cenBox text = leafBox (length text + 1) rex
  where
    rex = N SHUT_PREFIX "%" [T BARE_WORD text Nothing] Nothing

{-
natRex :: Nat -> GRex v
natRex n = case natUtf8 n of
    Left _                                   -> nameRex (tshow n)
    Right s | n < 256                        -> nameRex (tshow n)
            | all isNameChar s && length s>1 -> wrapCol (nameRex s)
            | isLineStr s                    -> T THIN_LINE s Nothing
            | isBlocStr s                    -> toLine s
            | otherwise                      -> nameRex (tshow n)
  where
    wrapCol r = N SHUT_PREFIX "%" [r] Nothing

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
-}

{-
xtagHasIdn :: XTag -> Bool
xtagHasIdn (XTAG 0 _) = False
xtagHasIdn _          = True
-}

simpleBody :: XBod -> Bool
simpleBody XVAR{}         = True
simpleBody (XRAW XVNAT{}) = True
simpleBody (XRAW XVCOW{}) = True
simpleBody _              = False

-- TODO Explicitly check if recursive somehow.
--         If we have a self-reference, we need to use the `?` form.
--         So, actually that check should happen in `resugar`
--         And the XLAW type should explicitly say (no name vs name).
lawBox :: XLaw -> Box
lawBox = \case
    XLAM r b ->
         let rb = argsBox r
         in boxNode "&" (mode, [rb, bb], NONE)
                        ([rb], Just bb)
      where
        bb = bodBox b
        mode = if simpleBody b && length r < 2
               then SHUT_INFIX
               else NEST_INFIX
    XLAW t r b ->
        let sb = sigBox t r
        in boxNode "?" (NEST_INFIX, [sb,bb], NONE)
                       ([sb], Just bb)
      where bb = bodBox b

boxNode :: Text
    -> (RuneShape, [Box], Maybe Box)
    -> ([Box], Maybe Box)
    -> Box
boxNode roon (wStyle, wKids, wHeir) (tKids, tHeir) =
    BOX siz wid tal
  where
    wid = N wStyle roon (boxWide <$> wKids) (boxWide <$> wHeir)
    tal = N OPEN   roon (box <$> tKids)     (box <$> tHeir)
    nestSz = 1 + length roon + length wKids + sum (boxWidth <$> wKids)
    siz = case wStyle of
            OPEN        -> nestSz
            NEST_PREFIX -> nestSz
            SHUT_PREFIX ->
                case (wKids, wHeir) of
                    ([k], Nothing) -> 1 + boxWidth k
                    _              -> nestSz


            SHUT_INFIX ->
                case (tKids <> toList tHeir) of
                    []  -> 2 + length roon              -- fallback to nestPrefx
                    [x] -> 3 + length roon + boxWidth x -- fallback to nestPrefx
                    xs  -> length roon * (length xs - 1) + sum (boxWidth<$>xs)

            NEST_INFIX ->
                -- TODO WAAAY overly simplistic
                let nk = length wKids in
                if nk<2 then nestSz else
                (2 + ((nk-1) * (2+length roon))
                   + sum (boxWidth <$> wKids)
                   + maybe 0 boxWidth wHeir
                )

keyBox :: Nat -> Box
keyBox 0   = nameBox "0"
keyBox nat =
    case natUtf8 nat of
        Right n | okIdn n -> nameBox n
        Right t | okTxt t -> cordBox t
        _                 -> natBox nat

okTxt :: Text -> Bool
okTxt txt =
    all isPrint txt && not (hasTic && hasQuo)
  where
    hasTic = elem '\'' txt
    hasQuo = elem '"'  txt

unCell :: [XVal] -> XVal -> [XVal]
unCell acc = \case
    XVAPP f x -> unCell (x:acc) f
    x         -> (x:acc)

parens :: [GRex v] -> GRex v
parens rexz = N NEST_PREFIX "|" rexz Nothing

pattern NONE :: Maybe a
pattern NONE = Nothing

runeSeq :: Text -> [GRex v] -> GRex v
runeSeq _   []     = error "impossible"
runeSeq ryn [x]    = N OPEN ryn [x] Nothing
runeSeq ryn (x:xs) = N OPEN ryn [x] (Just $ runeSeq ryn xs)

openSeq :: Text -> [[GRex Box]] -> GRex Box
openSeq roon = go
  where
    go []     = N OPEN roon [] Nothing
    go [x]    = N OPEN roon x Nothing
    go (x:xs) = N OPEN roon x (Just $ go xs)

simpleAppBox :: [Box] -> Box
simpleAppBox kids =
    BOX siz wid tal
  where
    siz = 1 + length kids + sum (boxWidth <$> kids)
    tal = N OPEN        "|" (box <$> kids) Nothing
    wid = N NEST_PREFIX "|" (boxWide <$> kids) Nothing

sigBox :: XTag -> (NonEmpty Symb) -> Box
sigBox n (r:|rs) = simpleAppBox (xtagBox n : fmap symbBox (r:rs))

argsBox :: (NonEmpty Symb) -> Box
argsBox (n:|[]) = symbBox n
argsBox (n:|ns) = simpleAppBox (symbBox <$> (n:ns))


{-
    TODO Right now `$a == a`
    TODO Make $a_3 be a shorthand for "a_3$a".
-}
xtagBox :: XTag -> Box
xtagBox = \case
    XTAG n Nothing  -> symbBox n
    XTAG n (Just t) -> BOX wd rx (fmap absurd rx)
        where nb = keyBox n
              tb = keyBox t
              rx = rn SHUT_INFIX "^" [nb,tb] NONE
              wd = boxWidth nb + 1 + boxWidth tb
  where
    rn :: RuneShape -> Text -> [Box] -> Maybe Box -> GRex Void
    rn m r k h = N m r (boxWide <$> k) (boxWide <$> h)

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

unXApp :: [XBod] -> XBod -> [XBod]
unXApp acc = \case
    XAPP f x -> unXApp (x:acc) f
    x        -> (x:acc)

bodBox :: XBod -> Box
bodBox = \case
    XVAR v           -> symbBox v
    XRAW x           -> valBox x
    XCNS (XVAPP f x) -> boxShutInfix "!" (valBox <$> unCell [x] f)
    XBAD (XVAPP f x) -> boxShutInfix ":" (valBox <$> unCell [x] f)
    XCNS x           -> boxPrefix "!" (valBox x)
    XBAD x           -> boxPrefix ":" (valBox x)
    XAPP f x         -> bodApply (unXApp [x] f)
    XLET n v k       -> letBox n v k

letBox :: Symb -> XBod -> XBod -> Box
letBox n v k =
    BOX siz wid tal
  where
    (siz, wid) =
        let nb = symbBox n
            bb = bodBox v
            kb = bodBox k
            kz = [nb,bb]
            wd = 5 + sum (boxWidth <$> [nb,bb,kb])
              -- TODO Except if the value-expression is an application,
              -- which will be then be unpacked (subtract 2)
              -- Also, need to check if continuation will be wrapped,
              -- and add 2 if so.
        in
            (wd, N NEST_INFIX "@" (boxWide <$> kz) (Just $ boxWide kb))

    tal = N OPEN "@" (box <$> [symbBox n, bodBox v])
                     (Just $ boxCont $ bodBox k)

boxCont :: Box -> GRex Box
boxCont = box . forceBoxOpen

bodApply :: [XBod] -> Box
bodApply exprs =
    case (sum widths < 60, reverse boxes) of
      (True, _)    -> boxApply boxes
      (False, oll) -> case span (\x -> boxWidth x < 10) oll of
                          ([], _) -> boxApply boxes
                          (_, []) -> boxApply boxes
                          (r, b)  -> dotBox (reverse r) (reverse b)
  where
    widths = boxWidth <$> boxes
    boxes = bodBox <$> exprs

dotBox :: [Box] -> [Box] -> Box
dotBox args body =
    foo { boxTall = tal }
  where
    foo = boxApply (body <> args)
    tal = N OPEN "." (box <$> args) (Just $ box $ boxApply body)

boxApply :: [Box] -> Box
boxApply []    = BOX 2 (N NEST_PREFIX "|" [] Nothing)
                       (N NEST_PREFIX "|" [] Nothing)
boxApply [bix] = bix
boxApply boxes =
    BOX siz wid tal
  where
    rexz = boxWide <$> boxes

    hepSz = (length boxes - 1) + sum (boxWidth<$>boxes)
    barSz = 1 + length boxes + sum (boxWidth<$>boxes)

    (siz, wid) =
        if all simple rexz
        then (hepSz, N SHUT_INFIX  "-" rexz Nothing)
        else (barSz, N NEST_PREFIX "|" rexz Nothing)

    simple (T BARE_WORD _ Nothing) = True
    simple _                       = False

    tal = case reverse boxes of
        []     -> N OPEN "|" [] Nothing
        [x]    -> N OPEN "|" [C x NONE] Nothing
        k:l:sx -> N OPEN "|" (box <$> reverse (l:sx))
                             (Just $ box $ forceBoxOpen k)


boxPrefix :: Text -> Box -> Box
boxPrefix roon expr =
    BOX siz wid tal
  where
    siz = length roon + boxWidth expr -- TODO Unless it needs to be (wrapped).
    wid = N SHUT_PREFIX roon [boxWide expr] NONE
    tal = N OPEN        roon [box expr]     NONE

boxShutInfix :: Text -> [Box] -> Box
boxShutInfix roon exprs =
    BOX siz wid tal
  where
    siz = case exprs of
            []  -> 2 + length roon              -- fallback to nest prefix
            [x] -> 3 + length roon + boxWidth x -- fallback to nest prefix
            xs  -> length roon * (length xs - 1) + sum (boxWidth<$>xs)
    wid = N SHUT_INFIX roon (boxWide <$> exprs) NONE
    tal = runeSeq roon (box <$> exprs)

-- TODO Hack for printing in ReplExe -------------------------------------------

xtagApp :: XTag -> NonEmpty Symb -> Rex
xtagApp t as = parens (xtagRex t : fmap symbRex (toList as))

xtagRex :: XTag -> Rex
xtagRex = \case
    XTAG n Nothing  -> keyRex n
    XTAG n (Just t) -> N SHUT_INFIX "$" [keyRex n, keyRex t] NONE

keyRex :: Symb -> Rex
keyRex = boxWide . keyBox

symbRex :: Symb -> Rex
symbRex symb =
    case (symb, natUtf8 symb) of
        (0, _)                   -> buk (nameRex "0")
        (_, Right nm) | okIdn nm -> nameRex nm
        (_, Right nm) | okTxt nm -> buk (cordRex nm)
        _                        -> buk (nameRex (tshow symb))
  where
    buk n = N SHUT_PREFIX "$" [n] Nothing
    cordRex = boxWide . cordBox

-- Parsing ---------------------------------------------------------------------

rexCmd :: ∀a. Show a => Rex -> Either Text (XCmd a)
rexCmd rex = resultEitherText id rex (runReading readCmd rex)

runReading :: Red v a -> GRex v -> Result v a
runReading act = unsafePerformIO . runResultT . runReadT act

resultEitherText :: (GRex v -> Rex) -> Rex -> Result v a -> Either Text a
resultEitherText cvt blk res =
    let ?rexColors = NoColors in
    case resultEither res of
      Right x       -> pure x
      Left (mR,err) -> Left (errMsg <> maybe "" showForm mR <> showBlock)
        where
          errMsg      = unlines["== READER ERROR ==", "", err ]
          showForm rx = unlines["In Form:" , "" , dent "  " (rexFile $ cvt rx) ]
          showBlock   = unlines["In Block:" , "" , dent "  " (rexFile blk) ]

dent :: Text -> Text -> Text
dent pre = unlines . fmap dentLine . lines
  where
    dentLine :: Text -> Text
    dentLine "" = pre
    dentLine ln = pre <> "  " <> ln

readCmd :: Red v (XCmd a)
readCmd = asum
    [ do rune "??"
         checks <- slip1N "??" readVal readVal
         pure $ XASSERT (checks <&> \(v,vs) -> foldl' XVAPP v vs)
    , do rune "<"
         (v,vs) <- form1Nc readVal readVal
         pure (XDUMPIT $ foldl' XVAPP v vs)
    , do rune "="
         binds <- slip2 "=" readBind readBod
         XDEFINE <$> traverse (uncurry mkBind) binds
    , XOUTPUT <$> readVal
    ]

mkBind :: XBindHead -> XBod -> Red v XBind
mkBind hed bod = case hed of
    XHEAD_PL t xs -> pure $ XBIND_PL (XLAW t xs bod)
    XHEAD_VL t    -> XBIND_VL t <$> bodCastToVal bod
    XHEAD_PN t    -> XBIND_PN t <$> bodCastToVal bod

bodCastToVal :: XBod -> Red v XVal
bodCastToVal = go
  where
    go = \case
        XRAW v   -> pure v
        XCNS v   -> pure v
        XBAD v   -> pure v
        XVAR t   -> pure (XVREF t)
        XAPP f x -> XVAPP <$> go f <*> go x
        XLET{}   -> throwError "No lets in value-definitions"

readBind :: Red v XBindHead
readBind = asum
    [ XHEAD_VL <$> readBymb
    , do (rune "|" <|> rune "-")
         asum [ XHEAD_PN <$> form1 readBymb
              , form2N readXTag readSymb readSymb <&> \(t,x,xs) -> XHEAD_PL t (x:|xs)
              ]
    ]

cenLit :: Red v XVal
cenLit = do
    rune "%"
    form1 $ asum
        [ XVNAT . utf8Nat <$> readName
        , readWideTabish
        ]

readVal :: Red v XVal
readVal = asum
    [ cenLit
    , XVNAT <$> readNat
    , XVCOW <$> readCow
    , XVREF <$> readSymb
    , XVBAR <$> readBar
    , rForm2c "?" readSigy readBod lamb
    , rForm2c "&" readArgs readBod anon
    , rFormNc "," readVal (XVROW . fromList)
    , rune "%%" >> readOpenTabish
    , do rune ",,"
         sequ <- slip1N ",," readVal readVal
         pure $ XVROW $ fromList $ fmap (uncurry xvap) sequ
    , rFormN1c "." readVal readVal appTo
    , do (rune "|" <|> rune "-")
         (x, xs) <- form1Nc readVal readVal
         let cell a []    = a
             cell a (b:c) = cell (XVAPP a b) c
         pure (cell x xs)
    ]
  where
    appTo xs f = apple f xs

    apple f []    = f
    apple f (b:c) = apple (XVAPP f b) c

    lamb (x,a) b = XVLAW (XLAW x a b)
    anon rs c    = XVLAW (XLAM rs c)
    xvap = foldl' XVAPP

-- TODO Tabs now use `=key` an cabs just use `key`.
readOpenTabish :: Red v XVal
readOpenTabish = do
    rex <- readRex
    case rex of
        N _ "%%" (N _ "=" [_] _ : _) _ -> readOpenTab
        _                              -> XVCAB <$> readOpenCab

readOpenTab :: Red v XVal
readOpenTab = do
    pairs <- slip1N "%%" (rune "=" >> form1 readKey) readVal
    let keySet = setFromList (fst <$> pairs) :: Set Nat
    when (length pairs /= length keySet) do
        -- TODO Find and show the offending key.
        throwError "Duplicated Key"
    let tab = mapFromList $ turn pairs \case
            (k, [])   -> (k, XVREF k)
            (k, x:xs) -> (k, foldl' XVAPP x xs)
    pure (XVTAB tab)

readOpenCab :: Red v (Set Nat)
readOpenCab = do
    keyList <- slip1 "%%" readKey
    let keySet = setFromList keyList
    when (length keyList /= length keySet) do
        -- TODO Find and show the offending key.
        throwError "Duplicated Key"
    pure keySet

readWideTabish :: Red v XVal
readWideTabish = either XVCAB XVTAB <$> do
    rune ","

    let keyEle n = (n, XVREF n)

    res :: [Either Nat (Nat,a)]
        <- formN $ asum
                 [ Left <$> readKey
                 , Right <$> (rune "=" >> (    form2 readKey readVal
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
    sawBoth = throwError "Cannot mix %[a] and %[a=3] forms."
    dups k  = throwError ("Key appears twice in tab literal: " <> tshow k)

readKey :: Red v Nat
readKey = asum
    [ readNumb
    , readIdnt
    , utf8Nat <$> readCord
    ]

readArgs :: Red v (NonEmpty Symb)
readArgs = asum
    [ singleton <$> readSymb
    , do rune "|" <|> rune "-" <|> rune "!" -- TODO Indicate inline-ness
         (x,xs) <- form1N readSymb readSymb
         pure (x :| xs)
    ]

readSigy :: Red v (XTag, NonEmpty Symb)
readSigy = do
    rune "|" <|> rune "-" <|> rune "!" -- TODO Indicate inline-ness
    (t,x,xs) <- form2N readXTag readSymb readSymb
    pure (t, x:|xs)

rForm2c :: Text -> Red v a -> Red v b -> (a -> b -> c) -> Red v c
rForm2c r x y k = rune r >> form2c x y >>= \(a,b) -> pure (k a b)

rForm1c :: Text -> Red v a -> (a -> b) -> Red v b
rForm1c r x k = rune r >> form1c x >>= \a -> pure (k a)

rForm1 :: Text -> Red v a -> (a -> b) -> Red v b
rForm1 r x k = rune r >> form1 x >>= \a -> pure (k a)

rFormNc :: Text -> Red v a -> ([a] -> b) -> Red v b
rFormNc r x k = rune r >> formNc x >>= \a -> pure (k a)

rFormN1c :: Text -> Red v a -> Red v b -> ([a] -> b -> c) -> Red v c
rFormN1c r x y k = rune r >> formN1c x y >>= \(a,b) -> pure (k a b)

rForm3c :: Text -> Red v a -> Red v b -> Red v c -> (a -> b -> c -> d)
        -> Red v d
rForm3c r x y z k = rune r >> form3c x y z >>= \(a,b,c) -> pure (k a b c)

rForm2 :: Text -> Red v a -> Red v b -> (a -> b -> c) -> Red v c
rForm2 r x y k = rune r >> form2 x y >>= \(a,b) -> pure (k a b)

rForm1Nc :: Text -> Red v a -> Red v b -> (a -> [b] -> c) -> Red v c
rForm1Nc r x y k = rune r >> form1Nc x y >>= \(a,bs) -> pure (k a bs)


readCow :: Red v Nat
readCow = matchLeaf "R[0-9]+" \case
    (BARE_WORD, "R0") -> Just 0

    (BARE_WORD, (unpack -> ('R' : n : ns))) -> do
        guard (all C.isDigit (n:ns))
        guard (n /= '0')
        readMay (n:ns)
    _ -> Nothing

readBod :: Red v XBod
readBod = asum
    [ XRAW <$> cenLit
    , XRAW . XVNAT <$> readNat
    , XRAW . XVBAR <$> readBar
    , readCow <&> \case 0 -> XRAW (XVROW mempty)
                        n -> XRAW (XVCOW n)
    , XVAR <$> readSymb
    , rFormN1c "." readBod readBod appTo
    , rune "|" >> (uncurry xapp <$> form1Nc readBod readBod)
    , rune "-" >> (uncurry xapp <$> form1Nc readBod readBod)
    , do rune ":"
         asum
             [ uncurry xbad <$> form1N readVal readVal
             , xbab <$> slip1 ":" readVal
             ]
    , rune "!" >>
          asum
              [ uncurry xcns <$> form1N readVal readVal
              , xkak <$> slip1 "!" readVal
              ]
    , rune "@" >> (xlet <$> form3c readSymb readBod readBod)
    , rForm2c "?" readSigy readBod lamb
    , rForm2c "&" readArgs readBod anon
    , rune "%%" >> fmap XRAW readOpenTabish

    -- TODO Separate out vals that are read XRAW, to avoid this
    -- duplication.
    , rFormNc "," readVal (XRAW . XVROW . fromList)
    , do rune ",,"
         sequ <- slip1N ",," readVal readVal
         pure $ XRAW $ XVROW $ fromList $ fmap (uncurry xvap) sequ
    ]
  where
    appTo xs f = apple f xs

    -- TODO Foldl?
    apple f []    = f
    apple f (b:c) = apple (XAPP f b) c

    xlet = \(n,v,k) -> XLET n v k
    xbad = \v vx -> XBAD (xvap v vx)
    xbab = \case []   -> error "impossible"
                 v:vx -> xbad v vx
    xkak = \case []   -> error "impossible"
                 v:vx -> xcns v vx
    xcns = \v vx -> XCNS (xvap v vx)
    xvap = foldl' XVAPP
    xapp = foldl' XAPP
    lamb (t,as) b = XRAW $ XVLAW (XLAW t as b)
    anon as     c = XRAW $ XVLAW (XLAM as c)

readTag :: Red v XTag
readTag = (simpleTag <$> readIdnt) <|> (rune "$" >> bucTag)
  where
    bucTag = keyTag <|> explicitTag
    keyTag = simpleTag <$> readKey
    explicitTag = do
        (n,t) <- form2 readKey readKey
        pure (XTAG n (Just t))

readXTag :: Red v XTag
readXTag = asum
    [ idnXTag <$> readKey
    , rune "$" >> asum
          [ form2 readKey readKey <&> \(n,t) -> XTAG n (Just t)
          , form1 readKey         <&> \n     -> XTAG n Nothing
          ]
    ]
  where
    idnXTag n = XTAG n NONE

simpleTag :: Symb -> XTag
simpleTag t = XTAG t Nothing

readIdnt :: Red v Symb
readIdnt = utf8Nat <$> readIdnTxt

readIdnTxt :: Red v Text
readIdnTxt = matchLeaf "symbol" \case
    (BARE_WORD,n) | okIdn n -> Just n
    _                       -> Nothing

readSymb :: Red v Symb
readSymb = readIdnt <|> (rune "$" >> form1 readKey)

readBymb :: Red v Symb
readBymb = readKey <|> (rune "$" >> form1 readKey)

readName :: Red v Text
readName = matchLeaf "name" \case
    (BARE_WORD,n) -> Just n
    _             -> Nothing

readCord :: Red v Text
readCord = matchLeaf "cord" \case
    (THIN_CORD,t) -> Just t
    (THIC_CORD,t) -> Just t
    _             -> Nothing

readLine :: Red v Text
readLine = matchLeaf "page" \case
    (THIC_LINE,l) -> Just l
    (THIN_LINE,l) -> Just l
    _             -> Nothing

isNameChar :: Char -> Bool
isNameChar '_' = True
isNameChar c   = isAlphaNum c

readNat :: Red v Nat
readNat =
    readNumb <|> readColn <|> readTape <|> line
  where
    line = utf8Nat <$> readLine
    readTape = utf8Nat <$> readCord
    readColn = rune "%" >> form1 (utf8Nat <$> readName)

readNumb :: Red v Nat
readNumb =
    matchName "nat" \n ->
    case unpack n of
        '_':_ -> Nothing
        _     -> readMay (filter (/= '_') n)

readAnyText :: Red v Text
readAnyText = (readName <|> readCord <|> readLine)

-- TODO Use x#_ and b#_ syntax
readBar :: Red v ByteString
readBar = do
    rune "#"
    (n, k) <- form2 readName readAnyText
    case n of
        "b" -> pure (encodeUtf8 k)
        "x" -> case B16.decode (encodeUtf8 k) of
                   Left er -> throwError ("Invalid hex literal: " <> pack er)
                   Right x -> pure x
        _   -> throwError "Invalid # literal"
