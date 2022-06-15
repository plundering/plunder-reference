-- Output helpers

module Plun.Print where

import PlunderPrelude

import Data.ByteString.Base58
import Rainbow

-- TODO Try making the output a single line.  See how long the output would be.
redOut :: MonadIO m => Text -> m ()
redOut = liftIO
       . putChunk
       . bold
       . fore red
       . chunk
       . (<> "\n")

-- TODO Try making the output a single line.  See how long the output would be.
greenOut :: MonadIO m => Text -> m ()
greenOut = liftIO
         . putChunk
         . bold
         . fore green
         . chunk
         . (<> "\n")

-- TODO Try making the output a single line.  See how long the output would be.
yellowOut :: MonadIO m => Text -> m ()
yellowOut = liftIO
          . putChunk
          . bold
          . fore yellow
          . chunk
          . (<> "\n")

niceLns :: Bool -> [Text] -> Text
niceLns False []     = "\n"
niceLns True  []     = ""
niceLns tight (l:ls) =
    let tight' = oneLine l
        more  = niceLns tight' ls
    in case (tight, tight') of
            ( True  , True  ) -> ""   <> l <> "\n" <> more
            ( True  , False ) -> "\n" <> l <> "\n" <> more
            ( False , True  ) -> "\n" <> l <> "\n" <> more
            ( False , False ) -> "\n" <> l <> "\n" <> more

oneLine :: Text -> Bool
oneLine blk = not ('\n' `elem` blk)

dent :: Text -> Text -> Text
dent pre = unlines . fmap dentLine . lines
 where dentLine :: Text -> Text
       dentLine "" = pre
       dentLine ln = pre <> " " <> ln



decodeBtc :: Text -> ByteString
decodeBtc = fromMaybe (error "bad hash")
          . decodeBase58 bitcoinAlphabet
          . encodeUtf8

encodeBtc :: ByteString -> Text
encodeBtc = decodeUtf8 . encodeBase58 bitcoinAlphabet
