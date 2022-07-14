{-# OPTIONS_GHC -Wall   #-}
{-# OPTIONS_GHC -Werror #-}

module Sire.Macro
    ( loadRex
    , loadRow
    , rexVal
    )
where

import PlunderPrelude
import Rex
import Sire.Types
import Loot.Types (Val(..))

--------------------------------------------------------------------------------

cordV :: Text -> Val v
cordV = NAT . utf8Nat

rowV :: [Val v] -> Val v
rowV = ROW . fromList

loadRow :: Val Pln -> Either (Val Pln, Text) [Val Pln]
loadRow (ROW r) = Right (toList r)
loadRow vl      = Left (vl, "row")

-- Macros ----------------------------------------------------------------------

loadRex :: (Val Pln -> Pln) -> Val Pln -> Either (Val Pln, Text) (GRex Pln)
loadRex putVal =
    \node -> loadRow node >>= loadRexRow node
  where
    recur = loadRex putVal

    loadName (NAT (natUtf8 -> Right n)) = Right n
    loadName vl                         = Left (vl, "name")

    loadCord (NAT (natUtf8 -> Right n)) = Right n
    loadCord vl                         = Left (vl, "cord")

    loadRexRow :: Val Pln -> [Val Pln] -> Either (Val Pln, Text) (GRex Pln)
    loadRexRow node = \case
        [NAT 0, r, x] -> loadRexRow node [NAT 0, r, x, NAT 0]
        [NAT 1, n]    -> loadRexRow node [NAT 1, n, NAT 0]
        [NAT 2, c]    -> loadRexRow node [NAT 2, c, NAT 0]
        [NAT 3, p]    -> loadRexRow node [NAT 3, p, NAT 0]
        [NAT 4, z]    -> loadRexRow node [NAT 4, z, NAT 0]

        [NAT 0, rv, xsv, mkv] -> do
            rwne <- loadCord rv
            xs <- toList <$> (loadRow xsv >>= traverse recur)
            mK <- loadCont mkv
            pure (N OPEN rwne xs mK)

        -- TODO Change this to match new representation
        [NAT 1, n, k] -> T BARE_WORD <$> loadName n <*> loadCont k
        [NAT 2, n, k] -> T THIN_CORD <$> loadCord n <*> loadCont k
        [NAT 3, p, k] -> T THIN_LINE <$> loadCord p <*> loadCont k
        [NAT 4, z, k] -> C (putVal z) <$> loadCont k
        NAT n : _     -> Left (node, "Invalid rex node (key=" <> tshow n <> ")")
        _             -> Left (node, "Invalid rex node")

    loadCont (NAT 0) = pure Nothing
    loadCont mkv     = Just <$> recur mkv


rexVal :: GRex Pln -> Val Pln
rexVal = \case
    N _ rn xs Nothing      -> rowV[NAT 0, cordV rn, rowV(rexVal<$>xs)]
    T BARE_WORD n Nothing  -> rowV[NAT 1, cordV n]
    T THIN_CORD c Nothing  -> rowV[NAT 2, cordV c]
    T THIN_LINE p Nothing  -> rowV[NAT 3, cordV p]
    C bed Nothing          -> rowV[NAT 4, REF bed]

    N _ rn xs (Just k)     -> rowV[NAT 0, cordV rn, rowV(rexVal<$>xs), rexVal k]
    T BARE_WORD n (Just k) -> rowV[NAT 1, cordV n, rexVal k]
    T THIN_CORD c (Just k) -> rowV[NAT 2, cordV c, rexVal k]
    T THIN_LINE p (Just k) -> rowV[NAT 3, cordV p, rexVal k]
    C bed (Just k)         -> rowV[NAT 4, REF bed, rexVal k]

    T THIC_CORD c k   -> rexVal (T THIN_CORD c k) -- TODO
    T THIC_LINE p k   -> rexVal (T THIN_LINE p k)
