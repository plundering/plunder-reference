-- TODO: =VCHUNKS

{-# OPTIONS_GHC -Wall    #-}
{-# OPTIONS_GHC -Werror  #-}

module Plun.JetImpl (installJetImpls, jetImpls) where

import Plun.Jets
import Data.Bits
import Data.Maybe
import Plun.Print
import Plun.Eval
import PlunderPrelude hiding ((^))

import Data.ByteString.Builder (byteString, toLazyByteString)
import Data.Vector             ((!), (//))

import qualified Data.ByteString as BS
import qualified Data.Vector     as V
import qualified Natty           as Natty
import qualified PlunderPrelude

--------------------------------------------------------------------------------

{-
    Call this immediatly on executable startup.
-}
installJetImpls :: IO ()
installJetImpls = writeIORef vJetImpl jetImpls

--------------------------------------------------------------------------------

jetImpls :: Map Text Jet
jetImpls = mapFromList
  [ ( "dec"         , decJet     )
  , ( "add"         , addJet     )
  , ( "mul"         , mulJet     )
  , ( "sub"         , subJet     )
  , ( "bex"         , bexJet     )
  , ( "lte"         , lteJet     )
  , ( "lth"         , lthJet     )
  , ( "div"         , divJet     )
  , ( "mod"         , modJet     )
  , ( "aeq"         , aeqJet     )
  , ( "lsh"         , lshJet     )
  , ( "rsh"         , rshJet     )
  , ( "mix"         , mixJet     )
  , ( "dis"         , disJet     )
  , ( "con"         , conJet     )
  , ( "if"          , ifJet      )
  , ( "ifNot"       , ifNotJet   )
  , ( "eql"         , eqlJet     )
  , ( "trk"         , trkJet     )
  , ( "die"         , dieJet     )
  , ( "idx"         , idxJet     )
  , ( "get"         , getJet     )
  , ( "len"         , vlenJet    )
  , ( "weld"        , vweldJet   )
  , ( "map"         , vmapJet    )
  , ( "put"         , vputJet    )
  , ( "mut"         , vmutJet    )
  , ( "take"        , vtakeJet   )
  , ( "drop"        , vdropJet   )
  , ( "cat"         , vcatJet    )
  , ( "isBar"       , isBarJet   )
  , ( "barIdx"      , bIdxJet    )
  , ( "barWeld"     , barWeldJet )
  , ( "barCat"      , barCatJet  )
  , ( "barFlat"     , barFlatJet )
  , ( "w32"         , w32Jet     )
  , ( "add32"       , add32Jet   )
  , ( "mul32"       , mul32Jet   )
  , ( "div32"       , div32Jet   )
  , ( "and32"       , and32Jet   )
  , ( "or32"        , or32Jet    )
  , ( "xor32"       , xor32Jet   )
  , ( "lsh32"       , lsh32Jet   )
  , ( "rsh32"       , rsh32Jet   )
  , ( "sub32"       , sub32Jet   )
  , ( "ror32"       , ror32Jet   )
  , ( "rol32"       , rol32Jet   )
  , ( "cordFromRow" , implodeJet ) -- TODO: Renamed to `rowCord`?
  ]

--------------------------------------------------------------------------------

ifJet :: Jet
ifJet _ env = if toBit(env^1) then env^2 else env^3

ifNotJet :: Jet
ifNotJet _ env = if toBit(env^1) then env^3 else env^2

eqlJet :: Jet
eqlJet _ env =
    NAT $ bool 0 1 $ (fastValEq (env^1) (env^2))

trkJet :: Jet
trkJet _ env = unsafePerformIO $ do
    s <- readIORef vShowPlun
    o <- s (env^1)
    greenOut o
    pure (env^2)

aeqJet :: Jet
aeqJet _ env = NAT $ bool 0 1 $ (toNat(env^1) == toNat(env^2))

lthJet :: Jet
lthJet _ env = if (toNat(env^1) <  toNat(env^2)) then NAT 1 else NAT 0

lteJet :: Jet
lteJet _ env = if (toNat(env^1) <= toNat(env^2)) then NAT 1 else NAT 0

bexJet :: Jet
bexJet _ env = NAT (bex $ toNat (env^1))

modJet :: Jet
modJet _ env = NAT (toNat(env^1) `mod` toNat(env^2))

addJet :: Jet
addJet _ env = NAT (toNat(env^1) + toNat(env^2))

lshJet :: Jet
lshJet _ env =
    let xv = toNat(env^1)
        yv = toNat(env^2)
    in
        if yv > maxInt
        then error "TODO"
        else NAT (xv `shiftL` (fromIntegral yv :: Int))

rshJet :: Jet
rshJet _ env =
    let xv = toNat(env^1)
        yv = toNat(env^2)
    in
        if yv > maxInt
        then error "TODO"
        else NAT (xv `shiftR` (fromIntegral yv :: Int))

decJet :: Jet
decJet _ env =
    case env^1 of
        NAT 0 -> NAT 0
        NAT n -> NAT (n-1)
        _     -> NAT 0

mulJet :: Jet
mulJet _ env = NAT (toNat(env^1) * toNat(env^2))

mixJet :: Jet
mixJet _ env = NAT (toNat(env^1) `xor` toNat(env^2))

conJet :: Jet
conJet _ env = NAT (toNat(env^1)  .|.  toNat(env^2))

disJet :: Jet
disJet _ env = NAT (toNat(env^1)  .&. toNat(env^2))

divJet :: Jet
divJet _ env =
    let yv = toNat (env^2)
    in if (yv == 0)
       then NAT 0
       else NAT (toNat(env^1) `div` yv)

subJet :: Jet
subJet _ env =
    let (x,y) = (toNat(env^1), toNat(env^2))
    in NAT (if y>x then 0 else (x-y))

vcatJet :: Jet
vcatJet f env = orExec (f env) do
      vs <- getRow (env^1)
      xs <- for vs getRow
      pure $ ROW $ concat xs

-- TODO Just don't do this, use bars instead of rows of bytes.
--
-- We don't accept 0 bytes, since their behavior in the plunder
-- implementation is weird (silently dropped)
--
-- TODO Converting from a vector to a list to a bytestring to an atom
-- is stupid `Natty` should be extended to support `Vector Word8`.
implodeJet :: Jet
implodeJet f env = orExec (f env) $ do
      vs <- getRow (env^1)
      bs <- for vs \case
          (NAT n) | n>0 && n<256 -> Just (fromIntegral n)
          _                      -> Nothing
      pure $ NAT $ Natty.bytesNat $ pack $ toList bs

-- Since we need to convert the index to an `Int`, we need to be
-- careful about overflow (input bigger than INTMAX is possible).
-- Thus we compare to the length manually, knowing that a vector of
-- size>INTMAX is impossible. (TODO True on all archs?)
vidx :: Nat -> Vector Fan -> Fan
vidx ind vec =
    let siz = fromIntegral (length vec)
    in if (ind >= siz)
       then NAT 0
       else vec ! fromIntegral ind

idxJet :: Jet
idxJet f env =
    orExec (f env) $ vidx (toNat $ env^1) <$> getRow (env^2)

getJet :: Jet
getJet f env =
    orExec (f env) $ vidx (toNat $ env^2) <$> getRow (env^1)

vlenJet :: Jet
vlenJet f env =
    orExec (f env) $ NAT . fromIntegral . length <$> getRow (env^1)

-- TODO: vsplice

vweldJet :: Jet
vweldJet f env =
    orExec (f env) (vweld <$> getRow (env^1) <*> getRow (env^2))
  where
    vweld :: Vector Fan -> Vector Fan -> Fan
    vweld x y = ROW (x ++ y)

vmapJet :: Jet
vmapJet f env =
    orExec (f env) (vmap (env^1) <$> getRow (env^2))
  where
    vmap :: Fan -> Vector Fan -> Fan
    vmap fun vec = ROW $ fmap (fun %%) vec

  -- TODO: vfind

vput :: Nat -> Fan -> Vector Fan -> Fan
vput ix vl rw =
    let !siz = fromIntegral (length rw)
    in ROW $ if (ix >= siz)
             then rw
             else rw // [(fromIntegral ix, vl)]

vputJet :: Jet
vputJet f env =
    orExec (f env) do
        rw <- getRow (env^1)
        let ix = toNat (env^2)
        let vl = env^3
        pure (vput ix vl rw)

vmutJet :: Jet
vmutJet f env =
    orExec (f env) do
        let ix = toNat (env^1)
            vl = (env^2)
        rw <- getRow (env^3)
        pure (vput ix vl rw)

vtakeJet :: Jet
vtakeJet f env =
    orExec (f env) (vtake (toNat (env^1)) <$> getRow (env^2))
  where
    vtake :: Nat -> Vector Fan -> Fan
    vtake n vec =
        let siz = fromIntegral (length vec)
        in ROW $ if (n >= siz)
                 then vec
                 else V.take (fromIntegral n) vec

vdropJet :: Jet
vdropJet f env =
    orExec (f env) (vdrop (toNat (env^1)) <$> getRow (env^2))
  where
    vdrop :: Nat -> Vector Fan -> Fan
    vdrop n vec =
      let siz = fromIntegral (length vec)
      in ROW $ if (n >= siz)
               then V.empty
               else V.drop (fromIntegral n) vec

bIdxJet :: Jet
bIdxJet f env =
    orExec (f env) (bidx (toNat (env^1)) <$> getBar (env^2))
  where
    bidx :: Nat -> ByteString -> Fan
    bidx n bs =
        let siz = fromIntegral (length bs)
        in NAT $ if (n >= siz)
                 then 0
                 else fromIntegral $ BS.index bs $ fromIntegral n

barCatJet :: Jet
barCatJet f env =
    orExecTrace "barCat" (f env) $ do
        vs <- getRow (env^1)
        bs <- traverse getBar vs
        pure $ BAR $ concat bs

isBarJet :: Jet
isBarJet _ env =
    case env^1 of
        BAR _ -> NAT 1
        _     -> NAT 0

barFlatJet :: Jet
barFlatJet _ env =
    BAR $ toStrict $ toLazyByteString $ go $ (env^1)
  where
    go (BAR b)   = byteString b
    go (ROW r)   = concat (go <$> r)
    go (TAB r)   = concat (go <$> toList r)
    go FUN{}     = mempty
    go NAT{}     = mempty
    go _         = error "TODO"

barWeldJet :: Jet
barWeldJet f e =
    orExecTrace "barWeld" (f e)
        (bweld <$> getBar (e^1) <*> getBar (e^2))
  where
    bweld :: ByteString -> ByteString -> Fan
    bweld a b = BAR (a <> b)

dieJet :: Jet
dieJet _ env = unsafePerformIO $ do
    s <- readIORef vShowPlun
    o <- s (env^1)
    redOut $ ("\n" <> o <> "\n")
    throwIO (USER_ERROR (env^1))

w32Jet :: Jet
w32Jet _ env =
    NAT (fromIntegral . w32 $ toNat(env^1))

w32op :: (Word32 -> Word32 -> Word32) -> Jet
w32op fun _ env = NAT $ fromIntegral $ fun (w32 $ toNat(env^1)) (w32 $ toNat(env^2))

add32Jet,mul32Jet,div32Jet,sub32Jet,and32Jet,xor32Jet,or32Jet :: Jet
add32Jet = w32op (+)
mul32Jet = w32op (*)
div32Jet = w32op (div)
sub32Jet = w32op (-)
and32Jet = w32op (.&.)
xor32Jet = w32op xor
or32Jet  = w32op (.|.)

{-# INLINE w32opInt #-}
w32opInt :: (Word32 -> Int -> Word32) -> Jet
w32opInt fun _ env =
    NAT $ fromIntegral $ fun (w32 $ toNat (env^1))
                             (fromIntegral $ w32 $ toNat (env^2))

lsh32Jet, rsh32Jet, ror32Jet, rol32Jet :: Jet
rol32Jet = w32opInt rotateL
lsh32Jet = w32opInt shiftL
rsh32Jet = w32opInt shiftR
ror32Jet = w32opInt rotateR


-- Utils -----------------------------------------------------------------------

toBit :: Fan -> Bool
toBit (NAT 0) = False
toBit (NAT _) = True
toBit _       = False

-- w32 helpers
bex32 :: Nat
bex32 = 2 PlunderPrelude.^ (32::Nat)

_w32max :: Nat
_w32max = bex32 - 1

maxInt :: Nat
maxInt = fromIntegral (maxBound::Int)

w32 :: Nat -> Word32
w32 x = fromIntegral (x `mod` bex32)

bex :: Nat -> Nat
bex n = 2 PlunderPrelude.^ n

getBar :: Fan -> Maybe ByteString
getBar (BAR b) = Just b
getBar _       = Nothing

orExec :: Fan -> Maybe Fan -> Fan
orExec _  (Just r) = r
orExec fb Nothing  = fb

orExecTrace :: Text -> Fan -> Maybe Fan -> Fan
orExecTrace _ fb res = orExec fb res

--  orExecTrace msg core xs res = case res of
--      Nothing -> trace (msg <> ".nomatch") (orExec core xs res)
--      Just{}  -> trace (msg <> ".match")   (orExec core xs res)
