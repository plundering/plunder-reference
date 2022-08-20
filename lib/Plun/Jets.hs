{-# OPTIONS_GHC -Wall   #-}
{-# OPTIONS_GHC -Werror #-}

module Plun.Jets
    ( vShowPlun
    , jetMatch
    , getRow
    , getByte
    , vJetHash
    , vJetImpl
    , Exe
    , Jet
    , UserError(..)
    )
where

import Data.Maybe
import Plun.Print
import Plun.Eval
import PlunderPrelude hiding ((^), traceM)
import Data.Primitive.SmallArray
-- ort Debug.Trace (traceM)

import Data.Text.IO (hPutStrLn)


-- Types -----------------------------------------------------------------------

type Exe = (SmallArray Fan -> Fan)
type Jet = (Exe -> Exe)

data UserError = USER_ERROR Fan
  deriving (Exception)

instance Show UserError where
    show (USER_ERROR e) = unsafePerformIO $ do
        s <- readIORef vShowPlun
        o <- s e
        pure $ "DIE called with:\n\n" <> (unpack $ dent "   " o)


-- Globals ---------------------------------------------------------------------

{-
    Very jank hack.  Make sure that the jets-hashes table gets overwritten
    immediatly on startup the first time jet matching happens, we will
    commit to that result forever.
-}
vJetHash :: IORef (Map Text ByteString)
vJetHash = unsafePerformIO (newIORef mempty)

vJetImpl :: IORef (Map Text Jet)
vJetImpl = unsafePerformIO (newIORef mempty)

-- Application overwrites this on startup.
vShowPlun :: IORef (Fan -> IO Text)
vShowPlun = unsafePerformIO $ newIORef $ const $ pure "[PLUN]"


-- Table Construction ----------------------------------------------------------

matchJetsToHash
    :: Map Text ByteString
    -> [(Text, Exe->Exe)]
    -> [(Text, Exe->Exe, ByteString)]
matchJetsToHash tab =
    mapMaybe \(t,x) -> do
        bs <- case lookup t tab of
                  Just r  -> Just r
                  Nothing -> do
                      _ <- error ("No hash corresponding to jet: " <> unpack t)
                      Nothing
        pure (t,x,bs)

table :: [(Text, (Exe -> Exe), ByteString)]
table = unsafePerformIO do
    matchJetsToHash <$> readIORef vJetHash
                    <*> (mapToList <$> readIORef vJetImpl)

jetsByHash :: Map ByteString (Text, Exe -> Exe)
jetsByHash = mapFromList (table <&> \(n,f,h) -> (h,(n,f)))

allJetNames :: Set Text
allJetNames = setFromList $ fmap (\(n,_,_) -> n) table


-- Jet Matching ----------------------------------------------------------------

jetMatch :: Pin -> Pin
jetMatch b@(P _ref _blob haz core _exe) = unsafePerformIO $ do
    let hashText = encodeBtc haz
    let name = case core of
                 FUN l -> lawNameText $ lawName l
                 _     -> ""
    if not (member name allJetNames)
    then pure b
    else do
        -- yellowOut name
        case lookup (pinHash b) jetsByHash of
            Just (nm, _) | nm /= name -> do
                hPutStrLn stderr name
                hPutStrLn stderr ("\t" <> tshow hashText)
                hPutStrLn stderr ("WRONG JET MATCHED: " <> nm)
                error "aborting"
            Nothing -> do
                hPutStrLn stderr (name <> "\tNOT MATCHED")
                hPutStrLn stderr ("    " <> tshow hashText)
                pure b
            Just (_nm, exe) -> do
                hPutStrLn stderr (name <> "\tMATCHED")
                let oldExec = pinExec b
                pure $ b { pinExec = \e -> exe oldExec e }


-- Utils -----------------------------------------------------------------------

getRow :: Fan -> Maybe (Vector Fan)
getRow (ROW xs) = Just xs
getRow _        = Nothing

getByte :: Fan -> Maybe Word8
getByte (NAT n) | n<256 = Just (fromIntegral n)
getByte _               = Nothing
