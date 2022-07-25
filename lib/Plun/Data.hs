{-# OPTIONS_GHC -Wall   #-}
{-# OPTIONS_GHC -Werror #-}

{- |
    We support 5 data jets.

    -   A ROW is an array
    -   A TAB is a dictionary (with nat keys)
    -   A BAR is a byteArray
    -   A COW is the constructor for a ROW.
    -   A CAB is the constructor for a TAB

    [Representation]:

        COW: Rn        = (0 1 (n+1) 0)
        CAB: %{...}    = (0 2 (n+1) {...}) where n=length([...])
        BAR: X'00ff00' = (0 1 1 0x0100ff00)
        ROW: {...}     = (Rn ...) where n=length([...])
        TAB: %{k=v ..} = (%{k ..} v ..)

    [Evaluation]:

        COW: (R2 x y)     -> {x y}
        CAB: (%{a b} x y) -> %{a=x b=y}
        BAR: X''       9  -> 0
        BAR: X'00ff00' 9  -> 0x0100ff00
        ROW: {x y} 9      -> R2
        TAB: %{k=v ...} 9 -> {k ...}
-}

module Plun.Data
    ( matchData
    , mkRow
    , mkBar
    , evalData
    )
where

import Plun.Types
import PlunderPrelude

import qualified Data.ByteString as BS
import qualified Data.Vector     as V

-- Data Jets -------------------------------------------------------------------

matchData :: (Pln -> Dat) -> LawName -> Nat -> Pln -> Maybe Dat
matchData _     (LN 0) 1 (PLN _ (NAT 0))       = Just $ ROW (fromList [])
matchData _     (LN 0) n (PLN _ (NAT 0))       = Just $ COW (n-1)
matchData _     (LN 0) n (PLN _ (DAT (ROW v))) = matchTab n v
matchData mkPin (LN 0) n (PLN _ x@APP{})       = mkPin <$> matchPin n x
matchData _     (LN 1) 1 (PLN _ (NAT n))       = matchBar n
matchData _     (LN _) _ _                     = Nothing

matchBar :: Nat -> Maybe Dat
matchBar n = do
    guard (n /= 0)
    let bitWidth = (natBitWidth n :: Nat) - 1
    guard (0 == (bitWidth `mod` 8))
    let bytWidth = fromIntegral (bitWidth `div` 8)
    pure $ BAR $ take bytWidth $ natBytes n

mkBar :: ByteString -> Pln
mkBar = PLN 1 . DAT . BAR

mkRow :: [Pln] -> Pln
mkRow = PLN 1 . DAT . ROW . V.fromList

matchPin :: Nat -> Nod -> Maybe Pln
matchPin 0 (APP (NAT 2) x)                             = Just x
matchPin 0 _                                           = Nothing
matchPin n (APP (APP (NAT 0) (PLN _ x)) (AT m)) | n==m = matchPin (n-1) x
matchPin _ _                                           = Nothing

matchTab :: Nat -> Vector Pln -> Maybe Dat
matchTab n vs = do
    guard (n == (1 + fromIntegral (length vs)))
    case toList vs of
        []        -> TAB <$> pure mempty
        AT a : es -> CAB <$> collect mempty a es
        _         -> Nothing
  where
    collect !acc i []                = pure (insertSet i acc)
    collect !acc i (AT w : ws) | w>i = collect (insertSet i acc) w ws
    collect _    _ _                 = Nothing

evalData :: [Pln] -> Dat -> Pln
evalData arg (PIN p) = pinExec p arg
evalData arg (COW _) = nodVal $ DAT $ ROW $ fromList arg
evalData arg (CAB k) = nodVal $ DAT $ TAB $ mapFromList $ zip (toList k) arg
evalData _   (ROW n) = nodVal $ DAT $ COW $ fromIntegral $ length n
evalData _   (TAB t) = nodVal $ DAT $ ROW $ fmap AT $ fromList $ keys t
evalData [x] (BAR b) = if null b then x else AT (barBody b)
evalData _   (BAR _) = error "impossible"

barBody :: ByteString -> Nat
barBody bytes =
    -- TODO Make this not slow
    bytesNat (bytes <> BS.singleton 1)
