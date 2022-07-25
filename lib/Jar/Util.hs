{-# OPTIONS_GHC -Wall   #-}
{-# OPTIONS_GHC -Werror #-}

module Jar.Util
    ( jarNoun
    , showJarBits
    , capJarTest
    )
where

import PlunderPrelude hiding (hash, (%))

import Jar
import Jar.Noun

-- Example Type and Instance for Testing ---------------------------------------

jarNoun :: Noun -> IO (Vector Hash256, ByteString)
jarNoun = jar

natBits :: Nat -> [Bool]
natBits 0 = []
natBits n = (1 == (n `mod` 2)) : natBits (n `div` 2)

showJarBits :: (Hashable a, Eq a, Show a, Nounable a, MonadIO m) => a -> m ()
showJarBits n = do
    (_, bs) <- jar n
    -- print bs
    -- print (unpack bs :: [Word8])
    let nat = bytesNat bs
    for_ (natBits nat) \x -> putChar (if x then '1' else '0')
    putStrLn ""

capJarTest :: (Show a, Nounable a) => a -> Either DecodeErr a
capJarTest v =
    let (refs, bits) = unsafePerformIO (jar v)
    in capBS (mkRefr <$> refs) bits
