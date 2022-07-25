--- OPTIONS_GHC -Wall   #-}
{-# OPTIONS_GHC -Werror #-}

{-|
    TODO: Use SmallCheck, QuickCheck, and Tasty.
-}
module Jar.TestExe where

import ClassyPrelude
import Jar
import Jar.Noun
import Jar.Util
import System.IO.Unsafe

import Test.Tasty
import Test.Tasty.QuickCheck as QC

-- Manual Testing --------------------------------------------------------------

a, b :: Noun
a = "aaaaaaaa"
b = "bbbbbbbb"
c = "cccccccc"
d = "dddddddd"

isPin :: Noun
isPin =
    ( 0
    % 474213282665
    % 1
    % ( 0
      % ( 2
        % ( 1
          % (0 % 0 % 1 % (2%1))
          % (0 % 0 % 3 % (2%0))
          % (0 % 0 % 2 % (2%0))
          % 0
          )
        )
      % 1
      )
    )

isPin' :: Noun
isPin' =
    ( (0 % 0)
    % (0 % 0 % 0 % (2%0))
    % (0 % 0 % 1 % (2%0))
    )


-- Tests -----------------------------------------------------------------------

vCount :: IORef Int
vCount = unsafePerformIO (newIORef 0)

takeTrip :: Noun -> IO (Maybe Noun)
takeTrip n = do
    case capJarTest n of
        Left err         -> error (show err)
        Right vl | vl==n -> pure Nothing
        Right vl         -> pure (Just vl)


checkTrip :: Noun -> IO ()
checkTrip n = do
    _ <- modifyIORef' vCount succ
    takeTrip n >>= \case
        Nothing -> pure ()
        Just vl -> do print n; print vl; error "NOT EQUALS"

main :: IO ()
main = do
    putStrLn "Running `jar` tests..."

    checkTrip 8
    checkTrip isPin'
    checkTrip isPin

    for_ [0..2048] (checkTrip . mkAtom)

    for_ [0..2048] \(mkAtom -> i) -> do
        checkTrip (i%i)
        checkTrip ((i%i) % (i%i))

    for_ [0..2048] \n -> do
        let i = mkAtom n
        let j = mkAtom (n*n*n)
        checkTrip ( ((i%i) % (i%a))
                  % j
                  % (5 % c % d)
                  % ((2%i) % b % (i%i))
                  )

    count <- readIORef vCount
    putStrLn ("OK!\n" <> tshow count <> " manual tests passed.")

    defaultMain $ testGroup "QuickCheck Tests"
        [ QC.testProperty "Jam/Cue roundtrip Noun" propJamCueNoun
       -- QC.testProperty "Jam/Cue roundtrip Plunder" propJamCuePlun
        ]

instance Arbitrary Hash256 where
    arbitrary = do
        flag <- (`mod` 4) <$> arbitrary @Int
        case flag of
            0 -> pure $ Hash256 $ pack $ replicate 32 1
            1 -> pure $ Hash256 $ pack $ replicate 32 2
            2 -> pure $ Hash256 $ pack $ replicate 32 3
            _ -> Hash256 . pack <$> replicateM 32 arbitrary

instance Arbitrary Noun where
    arbitrary = do
        i :: Int <- abs <$> arbitrary
        case (i `mod` 5) of
            0 -> L . fromIntegral . abs <$> arbitrary @Integer
            1 -> L . fromIntegral . abs <$> arbitrary @Integer
            2 -> R <$> arbitrary
            _ -> N <$> arbitrary <*> arbitrary

propJamCueNoun :: Noun -> Bool
propJamCueNoun a = unsafePerformIO (takeTrip a) == Nothing

-- propJamCuePlun :: Pln -> Bool
-- propJamCuePlun a = unsafePerformIO (takeTrip a) == Nothing
