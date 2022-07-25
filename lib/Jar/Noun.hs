{-# OPTIONS_GHC -Wall   #-}
{-# OPTIONS_GHC -Werror #-}

module Jar.Noun
    ( Noun(..)
    , (%)
    , nounable
    )
where

import PlunderPrelude hiding (hash, (%))

import Jar.Nounable


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

--------------------------------------------------------------------------------

nounable :: Nounable a => a -> Noun
nounable v =
    case nounView v of
        NATISH n   -> L n
        REFISH _ r -> R r
        APPISH f x -> N (nounable f) (nounable x)
