{-# OPTIONS_GHC -Wall   #-}
{-# OPTIONS_GHC -Werror #-}

module Rex.ReplExe (main) where

import PlunderPrelude
import Rex

--------------------------------------------------------------------------------

main :: IO ()
main = colorsOnlyInTerminal do
    forceOpen <- getArgs <&> \case "--open":_ -> True
                                   _          -> False
    let f = if forceOpen then open else id
    replStdin \case
        BLK _ _ (Left err) -> putStrLn (dent "!!" $ err)
        BLK _ _ (Right rx) -> putStrLn (rexFile $ f rx)

open :: Rex -> Rex
open (N _ r cs k) = N OPEN r (open <$> cs) (open <$> k)
open (T th t k)   = T th t (open <$> k)
open (C c _)      = absurd c

dent :: Text -> Text -> Text
dent pre =
    unlines . fmap dentLine . lines
  where
    dentLine :: Text -> Text
    dentLine "" = pre
    dentLine ln = pre <> "  " <> ln
