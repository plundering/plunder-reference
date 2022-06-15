{-# OPTIONS_GHC -Wall   #-}
{-# OPTIONS_GHC -Werror #-}

module Jar.Noun
    ( jarNoun
    , Noun(..)
    , (%)
    , showJarBits
    , capJarTest
    ) where

import PlunderPrelude hiding (hash, (%))

import Jar


-- Example Type and Instance for Testing ---------------------------------------

data Noun
    = L Nat
    | R Hash256
    | N Noun Noun
  deriving (Eq, Ord, Generic)

instance Show Noun where
    show (L n)   = show n
    show (N x y) = show (x, y)
    show (R h)   = show h

instance Nounable Noun where
    type NounRef Noun = Hash256

    mkCell = N
    mkAtom = L
    mkRefr = R

    nounView = \case
        L x   -> NATISH x
        R h   -> REFISH h h
        N x y -> APPISH x y

infixl 5 %

(%) :: Nounable a => a -> a -> a
(%) = mkCell

instance IsString Noun where
    fromString = R . Hash256 . encodeUtf8 . pack
    -- TODO Decode base$x and validate

instance Num Noun where
    fromInteger = mkAtom . fromInteger
    (+) = error "hack for number syntax"
    (*) = error "hack for number syntax"
    abs = error "hack for number syntax"
    signum = error "hack for number syntax"
    negate = error "hack for number syntax"

instance Hashable Noun where

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
