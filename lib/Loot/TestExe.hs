{-# OPTIONS_GHC -Wall   #-}
{-# OPTIONS_GHC -Werror #-}

module Loot.TestExe (main, validateLoot) where

import PlunderPrelude
import Rex
import Rex.TestUtils
import Test.Tasty
import Plun

import Loot.Types       (Symb)
import GHC.IO.Handle    (hFlushAll)
import System.Directory (removeFile)

import qualified Loot.ReplExe as Repl

--------------------------------------------------------------------------------

devNull :: Handle
devNull = unsafePerformIO (openFile "/dev/null" WriteMode)

main :: IO ()
main =
    let ?rexColors = NoColors in
    do vEnv <- liftIO $ newIORef mempty
       langTest ".loot" (goldenLoot vEnv)

cab,cabHash :: Nat
cab = utf8Nat "_"
cabHash = utf8Nat "_hash"

goldenLoot :: RexColor => IORef (Map Symb Pln) -> GoldPaths -> TestTree
goldenLoot vEnv pax = do
    runTest pax doBlk end
  where
    doBlk :: FilePath -> Handle -> Bool -> Block -> IO ()
    doBlk _fil _h _firstLn blk = Repl.runBlock devNull False vEnv blk

    GOLD_PATHS{gpSource,gpOutput} = pax

    end :: Handle -> IO ()
    end h = do
        env <- readIORef vEnv
        pln <- pure $ fromMaybe (AT 0) $ lookup cab env
        has <- pinHash <$> mkPin' pln
        Repl.printValue h False (Just cab) pln
        Repl.printValue h True (Just cabHash) (mkBar has)
        hFlushAll h >> hClose h
        validateLoot gpOutput (gpSource <> ".tmp")

validateLoot :: RexColor => FilePath -> FilePath -> IO ()
validateLoot lootFile tempFile = do
    vEnv <- liftIO (newIORef mempty)
    Repl.replFile lootFile (Repl.runBlock devNull False vEnv)
    h <- openFile tempFile WriteMode
    env <- readIORef vEnv
    pln <- pure $ fromMaybe (AT 0) $ lookup cab env
    has <- pinHash <$> mkPin' pln
    Repl.printValue h False (Just cab) pln
    Repl.printValue h True (Just cabHash) (mkBar has)
    hFlushAll h >> hClose h
    srcText <- readFileUtf8 lootFile
    tmpText <- readFileUtf8 tempFile
    unless (srcText == tmpText) do
        putStrLn ("Re-Processing output produces different result")
        putStrLn "To show the difference, run:\n"
        putStrLn $ concat
            [ "    diff --color '"
            , pack lootFile
            , "' '"
            , pack tempFile
            , "'"
            ]
        error "Roundtrip Failed"
    removeFile tempFile
