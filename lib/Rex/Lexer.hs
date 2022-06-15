{-# OPTIONS_GHC -Wall -Werror #-}

module Rex.Lexer
    ( lexLine
    , Frag(..)
    , Form(..)
    , Item(..)
    , Itmz
    , Nest(..)
    , Leaf(..)
    , isRuneChar
    , isNameChar
    )
where

import PlunderPrelude        hiding (many, some, try)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char

type Parser = Parsec Void Text

data Frag = RUNE Text
          | FORM Form
          | PAGE Bool Text
  deriving (Eq, Ord, Show)

data Form = BEFO Text Form | SHIN Itmz [(Text, Itmz)]
  deriving (Eq, Ord, Show)

data Item = LEAF Leaf | NEST Nest
  deriving (Eq, Ord, Show)

type Itmz = NonEmpty Item

data Nest
    = INFIX [Form] [(Text, [Form])]
    | PREFX Text [Form]
    | WRAPD Form
  deriving (Eq, Ord, Show)

data Leaf
    = N Text
    | C Bool Text
  deriving (Eq, Ord, Show)

spc :: Parser ()
spc = void $ char ' '

spc0 :: Parser ()
spc0 = void $ many spc

whyt :: Parser ()
whyt = void $ some spc

page :: Parser (Bool, Text)
page = do
    thic <- (string "\"\"\"" $> True)
        <|> (string "'''"    $> False)
    (thic,) <$> takeRest

isNameChar :: Char -> Bool
isNameChar = (`elem` ("_" <> ['a'..'z'] <> ['A'..'Z'] <> ['0'..'9']))

isRuneChar :: Char -> Bool
isRuneChar = (`elem` ("$!#%&*+,-./:<=>?@\\^`|~" :: String))

name :: Parser Text
name = label "name" (pack <$> some (satisfy isNameChar))

cord' :: Char -> Parser Text
cord' quo = pack <$> (char quo *> many(satisfy(/= quo)) <* char quo)

cord :: Parser (Bool, Text)
cord = ((True,)  <$> cord' '"')
   <|> ((False,) <$> cord' '\'')

para :: Parser Nest
para = do
    (pal >> spc0 >> faro) <&> \case
        [Right f] -> WRAPD f
        is        -> unravel [] is
  where
    (pal, par) = ( char '(' , char ')' )
    mixy = do { r <- try (rune <* whyt) ; (Left r :) <$> faro }
    term = par $> []
    faro = do
        i  <- Right <$> form
        is <- term <|> mixy <|> (whyt >> (term <|> mixy <|> faro))
        pure (i:is)

    ravel :: Text -> [Form] -> [Either Text Form] -> [(Text, [Form])]
    ravel r ws = \case
        []           -> (r, reverse ws) : []
        Left s  : is -> (r, reverse ws) : ravel s [] is
        Right w : is -> ravel r (w:ws) is

    unravel :: [Form] -> [Either Text Form] -> Nest
    unravel ws = \case
        []           -> INFIX (reverse ws) []
        Left  r : is -> INFIX (reverse ws) (ravel r [] is)
        Right w : is -> unravel (w:ws) is

rune :: Parser Text
rune = pack <$> some runic

runic :: Parser Char
runic = label "runic" (satisfy isRuneChar)

nest :: Parser Nest
nest = curl <|> brak <|> para

leaf :: Parser Leaf
leaf = (uncurry C <$> cord) <|> (N <$> name)

nestForm :: Nest -> Form
nestForm n = SHIN (NEST n :| []) []

brak :: Parser Nest
brak = ravel <$> (char '[' >> spc0 >> (empt <|> brok))
  where
    empt = [] <$ char ']'

    brok = (:) <$> frog <*> (empt <|> do{whyt; empt <|> brok})

    unravel roon ns = \case
        []          -> PREFX roon $ reverse ns
        Left r : is -> PREFX roon $ reverse (nestForm(unravel r [] is) : ns)
        Right w : is -> unravel roon (w:ns) is

    ravel frags =
        case frags of
           Left r : is -> unravel r   [] is
           is          -> unravel "|" [] is

-- This is the same as `frag` but does not accept pages.
frog :: Parser (Either Text Form)
frog = (rinse <$> rune <*> optional form)
   <|> (Right <$> form)
  where
    rinse :: Text -> Maybe Form -> Either Text Form
    rinse r = maybe (Left r) (Right . BEFO r)

curl :: Parser Nest
curl = char '{' >> spc0 >> (PREFX "," <$> (empt <|> carl))
  where
    empt = [] <$ char '}'
    carl = (:) <$> form <*> (empt <|> (do whyt; empt <|> carl))

itmz :: Parser Itmz
itmz = (:|) <$> item <*> many item

item :: Parser Item
item = (NEST <$> nest) <|> (LEAF <$> leaf)

shin :: Parser Form
shin = SHIN <$> itmz <*> many (try (ppair rune itmz))
  where
    ppair x y = (,) <$> x <*> y

form :: Parser Form
form = (BEFO <$> rune <*> shin) <|> shin

frag :: Parser (Int, Frag)
frag = (,) <$> getOffset
           <*> (  (uncurry PAGE <$> page)
              <|> (rinse <$> rune <*> optional form)
              <|> (FORM <$> form)
               )
  where
    rinse :: Text -> Maybe Form -> Frag
    rinse r = maybe (RUNE r) (FORM . BEFO r)

line :: Parser [(Int, Frag)]
line = spc0 >> (nada <|> pcons frag loan)
  where
    loan = nada <|> (whyt >> (nada <|> pcons frag loan))
    nada = ((char ';' >> takeRest) $> [])
       <|> (eof $> [])
    pcons x y = (:) <$> x <*> y

lexLine :: (FilePath, Int) -> Text -> Either Text [(Int, Frag)]
lexLine (pax, lin) txt = do
    case snd (runIdentity $ runParserT' line $ initialState pax lin txt)
      of Left  e -> Left (pack $ errorBundlePretty e)
         Right x -> Right x

initialState :: FilePath -> Int -> Text -> State Text e
initialState pax lin txt =
    State
    { stateInput = txt
    , stateOffset = 0
    , stateParseErrors = []
    , statePosState =
        PosState
        { pstateInput = txt
        , pstateOffset = 0
        , pstateTabWidth = mkPos 8
        , pstateLinePrefix = ""
        , pstateSourcePos = SourcePos pax (mkPos 1) (mkPos(lin+1))
        }
    }
