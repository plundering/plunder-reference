{-# OPTIONS_GHC -Wall   #-}
{-# OPTIONS_GHC -Werror #-}

module Sire.TestExe (main) where

import PlunderPrelude
import Rex
import Rex.TestUtils
import Test.Tasty
import Plun
import Sire.Types

import qualified Loot.ReplExe as Loot
import qualified Loot.TestExe as Loot
import qualified Plun         as P
import qualified Sire.ReplExe as Sire

import GHC.IO.Handle (hFlushAll)


-- Globals ---------------------------------------------------------------------

vMac :: IORef (Map Text Pln)
vMac = unsafePerformIO (newIORef mempty)

vLin :: IORef (Map Symb (Fun Pln Sire.Refr Sire.Global))
vLin = unsafePerformIO (newIORef mempty)

devNull :: Handle
devNull = unsafePerformIO (openFile "/dev/null" WriteMode)


--------------------------------------------------------------------------------

main :: IO ()
main = do
    writeIORef P.vShowPlun (pure . Sire.showPlun)
    modifyIORef' P.state \st -> st { P.stFast = P.jetMatch }

    -- TODO Need to reset this after each test.
    vEnv <- liftIO $ newIORef mempty
    langTest ".sire" (let ?rexColors = NoColors
                      in goldenLoot vEnv)

cab :: Symb
cab = utf8Nat "_"

goldenLoot :: RexColor => IORef (Map Symb Pln) -> GoldPaths -> TestTree
goldenLoot vEnv pax = do
    runTest pax doBlk end
  where
    GOLD_PATHS{gpSource,gpOutput} = pax

    doBlk :: FilePath -> Handle -> Bool -> Block -> IO ()
    doBlk _fil _h _firstLn blk = do
        let actor _ = pure (0, 0)
        Sire.runBlockPlun devNull False actor vEnv vMac vLin blk

    end :: Handle -> IO ()
    end h = do
           env <- readIORef vEnv
           pln <- pure $  fromMaybe (AT 0) $ lookup cab env
           has <- pinHash <$> mkPin' pln

           Loot.printValue h False (Just $ utf8Nat "_") pln
           Loot.printValue h True (Just $ utf8Nat "_hash") (mkBar has)
           hFlushAll h >> hClose h
           Loot.validateLoot gpOutput (gpSource <> ".tmp")
