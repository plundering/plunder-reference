{-# OPTIONS_GHC -Wall   #-}
{-# OPTIONS_GHC -Werror #-}

module Main where

import PlunderPrelude
import Rex
import Rex.TestUtils
import Test.Tasty

import Data.Text.IO (hPutStrLn, hPutStr)

--------------------------------------------------------------------------------

main :: IO ()
main = langTest ".rex" goldenRex

goldenRex :: GoldPaths -> TestTree
goldenRex pax = do
    runTest pax \fil h firstLn (BLK _pos tex blk) -> do
        unless firstLn (hPutStrLn h "")
        hPutStr h $ case blk of Left err -> dent "!!" err
                                Right vl -> verifyBlock fil tex vl

verifyBlock :: FilePath -> Text -> Rex -> Text
verifyBlock fil inp rex =
  let out = rexFile rex
  in case parseBlocks fil out of
       Right [r] | r==rex -> out
       Right rs           -> dent "!!" $ noRound inp rs
       Left err           -> dent "!!" $ roundErr inp err

noRound :: Text -> [Rex] -> Text
noRound inp bars =
  unlines [ "This expression:\n"
          , inp
          , "\nBecomes this when pretty-printed and re-parsed\n"
          , intercalate "\n" (rexFile <$> bars)
          ]

roundErr :: Text -> Text -> Text
roundErr inp err =
  unlines [ "This input:\n"
          , inp
          , "\nGets this error when pretty-printed and re-parsed\n"
          , err
          ]
