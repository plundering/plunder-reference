{-# OPTIONS_GHC -Wall   #-}
{-# OPTIONS_GHC -Werror #-}

module Main where

import PlunderPrelude

import Rex (replStdin, rexFile, Block(BLK))

--------------------------------------------------------------------------------

main :: IO ()
main = replStdin \case
    BLK _ _ (Left err) -> putStrLn (dent "!!" $ err)
    BLK _ _ (Right rx) -> putStrLn (rexFile rx)

dent :: Text -> Text -> Text
dent pre =
    unlines . fmap dentLine . lines
  where
    dentLine :: Text -> Text
    dentLine "" = pre
    dentLine ln = pre <> "  " <> ln
