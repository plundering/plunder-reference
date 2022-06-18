{-# OPTIONS_GHC -Wall   #-}
{-# OPTIONS_GHC -Werror #-}

module RexExe (main) where

import PlunderPrelude
import Rex

--------------------------------------------------------------------------------

main :: IO ()
main = replStdin \case
    BLK _ _ (Left err) -> putStrLn (dent "!!" $ err)
    BLK _ _ (Right rx) -> putStrLn (rexFileColor boldColoring $ open rx)

open :: Rex -> Rex
open (N _ r cs k) = N OPEN r (open <$> cs) (open <$> k)
open (T th t k) = T th t (open <$> k)
open (C c k)    = C (openAmb c) (open <$> k)

openAmb :: RexAmb -> RexAmb
openAmb (AS r ps) = AS (open r) (over _2 open <$> ps)
openAmb (AN r ps) = AN (open r) (over _2 open <$> ps)

dent :: Text -> Text -> Text
dent pre =
    unlines . fmap dentLine . lines
  where
    dentLine :: Text -> Text
    dentLine "" = pre
    dentLine ln = pre <> "  " <> ln
