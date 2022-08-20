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

vMac :: IORef (Map Text Fan)
vMac = unsafePerformIO (newIORef mempty)

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
                      in goldenSire vEnv)

cab :: Symb
cab = utf8Nat "_"

goldenSire :: RexColor => IORef (Map Symb Sire.Global) -> GoldPaths -> TestTree
goldenSire vEnv pax = do
    runTest pax doBlk end
  where
    GOLD_PATHS{gpSource,gpOutput} = pax

    doBlk :: FilePath -> Handle -> Bool -> Block -> IO ()
    doBlk _fil _h _firstLn blk = do
        let actor _ = pure (0, 0)
        Sire.runBlockPlun devNull False actor vEnv vMac blk

    end :: Handle -> IO ()
    end h = do
           env <- readIORef vEnv
           pln <- pure $ fromMaybe (NAT 0) (Sire.gPlun <$> lookup cab env)
           has <- pinHash <$> mkPin' pln

           Loot.printValue h False (Just $ utf8Nat "_") pln
           Loot.printValue h True (Just $ utf8Nat "_hash") (BAR has)
           hFlushAll h >> hClose h
           Loot.validateLoot gpOutput (gpSource <> ".tmp")
