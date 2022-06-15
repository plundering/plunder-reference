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

        COW: Rn         = (0 1 (n+1) 0)
        CAB: %{...}     = (0 2 (n+1) {...}) where n=length([...])
        BAR: bx'00ff00' = (0 3 1 (2 0xff00 1))
        ROW: {...}      = (Rn ...) where n=length([...])
        TAB: %{k=v ..}  = (%{k ..} v ..)

    [Evaluation]:

        COW: (R2 x y)     -> {x y}
        CAB: (%{a b} x y) -> %{a=x b=y}
        BAR: bx'00ff00' 0 -> (2 0xff00 1)
        ROW: {x y} 0      -> R2
        TAB: %{k=v ...} _ -> {k ...}
-}

module Plun.Data
    ( matchData
    , mkRow
    , mkBar
    , evalData
    , dataWut
    )
where

import Plun.Types
import PlunderPrelude

import Data.Map (toAscList)

import qualified Data.Set    as S
import qualified Data.Vector as V

-- Data Jets -------------------------------------------------------------------

matchData :: LawName -> Nat -> Val -> Maybe Dat
matchData (LN 1) 1 (AT 0)                = Just $ ROW (fromList [])
matchData (LN 1) n (AT 0)                = Just $ COW (n-1)
matchData (LN 2) n (VAL _ (DAT (ROW v))) = matchTab n v
matchData (LN 3) 1 (VAL _ (DAT (ROW v))) = matchBar v
matchData (LN _) _ _                     = Nothing

matchBar :: Vector Val -> Maybe Dat
matchBar (toList -> [AT b, AT z]) = Just $ BAR $ mkBarBS b z
matchBar _                        = Nothing

mkBarBS :: Nat -> Nat -> ByteString
mkBarBS b z = natBytes b <> replicate (fromIntegral z) 0

mkBar :: ByteString -> Val
mkBar = VAL 1 . DAT . BAR

mkRow :: [Val] -> Val
mkRow = VAL 1 . DAT . ROW . V.fromList

matchTab :: Nat -> Vector Val -> Maybe Dat
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

evalData :: [Val] -> Dat -> Val
evalData arg (COW _) = nodVal $ DAT $ ROW $ fromList arg
evalData arg (CAB k) = nodVal $ DAT $ TAB $ mapFromList $ zip (toList k) arg
evalData _   (ROW _) = AT 0
evalData _   (TAB _) = AT 0
evalData _   (BAR _) = AT 0

dataWut :: forall a . (LawName -> Nat -> Val -> a) -> (Val -> Val -> a) -> Dat -> a
dataWut rul cel = \case
    ROW v -> case toList v of
                    []   -> rul (LN 1) 1 (AT 0)
                    x:xs -> apple (DAT $ COW sz) (x :| xs)
               where sz = fromIntegral (length v)
    TAB d -> tabWut d
    BAR b -> rul (LN 3) 1 body
      where
        body = nodVal $ DAT $ ROW $ fromList $ [AT bits, AT zers]
        bits = bytesNat b
        zers = fromIntegral (length b - length b')
                 where b' = natBytes bits

    COW n -> rul (LN 1) (n+1) (AT 0)
    CAB k -> cabWut k
  where
    apple :: Nod -> NonEmpty Val -> a
    apple n (x :| [])     = cel (nodVal n) x
    apple n (v :| (x:xs)) = apple (APP n v) (x :| xs)

    tabWut :: Map Nat Val -> a
    tabWut tab = case val of
                    []   -> cabWut mempty
                    v:vs -> apple (DAT $ CAB key) (v :| vs)
      where par = toAscList tab
            key = setFromList (fst <$> par)
            val = snd <$> par

    cabWut :: Set Nat -> a
    cabWut ks = rul (LN 2) nArgs (nodVal $ DAT $ ROW $ AT <$> k)
      where nArgs = fromIntegral (length k+1)
            k     = fromList (S.toAscList ks)
