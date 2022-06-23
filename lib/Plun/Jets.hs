module Plun.Jets
    ( vShowPlun
    , jetMatch
    , getRow
    , getByte
    )
where

import Data.Bits
import Data.ByteString.Base58
import Data.Maybe
import Plun.Eval
import Plun.Print
import Plun.Types
import PlunderPrelude

import Plun.Data               (mkBar)
import Control.Monad.Except    (ExceptT, runExceptT)
import Control.Monad.Except    (MonadError, liftEither, throwError)
import Data.ByteString.Builder (byteStringHex, toLazyByteString)
import Data.Coerce             (coerce)
import Data.Text.IO            (hPutStrLn)
import Data.Vector             ((!), (//))
import Natty                   (Nat)
import System.IO.Unsafe        (unsafePerformIO)

import qualified Data.ByteString as BS
import qualified Data.Text       as T
import qualified Data.Vector     as V
import qualified Natty           as Natty

getRow :: Val -> Maybe (Vector Val)
getRow (VAL _ (DAT (ROW xs))) = Just xs
getRow _                      = Nothing

getByte :: Val -> Maybe Word8
getByte (VAL _ (NAT n)) | n<256 = Just (fromIntegral n)
getByte _                       = Nothing

-- Application overwrites this on startup.
vShowPlun :: IORef (Val -> IO Text)
vShowPlun = unsafePerformIO $ newIORef $ \v -> pure "[PLUN]"

-- w32 helpers
bex32 :: Nat
bex32 = 2 ^ (32::Nat)

_w32max :: Nat
_w32max = bex32 - 1

w32 :: Nat -> Word32
w32 x = fromIntegral (x `mod` bex32)

data UserError = UserError Val
  deriving (Exception)

instance Show UserError where
  show (UserError e) = unsafePerformIO $ do
    s <- readIORef vShowPlun
    o <- s e
    pure $ "DIE called with:\n\n" <> (unpack $ dent "   " o)

table :: [(Text, (Val -> [Val] -> Val), ByteString)]
table =
  [("add"      ,addJet      ,xx "DmQSboknWfMEi5VSrqzM5AkXNcmEJAagtGum48t9D5yY")
  ,("sub"      ,subJet      ,xx "B6MvfHoH5QAQRCkFB3aF2GkmWZY5FssjmKV4VmDyEHjY")
  ,("mul"      ,mulJet      ,xx "DLZ7asf6Z8JKVJh7XYdPE2vvchwLi8SYDqg1KdmRPGWM")
  ,("aeq"      ,aeqJet      ,xx "GpwM7oWc9qNujghF21rnG4eSPrbTYEdLvjTW8ET2WX2v")
  ,("bex"      ,bexJet      ,xx "3r98Xw8DcPxaa3fG5UVGPbPN7Bz4ZDYXQ98d1KcNLzhM")
  ,("mod"      ,modJet      ,xx "DZ9wrRW6rtcZsGPeVHBcdh9ohAz1NYuzVCApXVcExH5K")
  ,("div"      ,divJet      ,xx "HUUEAxqgVoFNd2zVFRPhFuaJYese1S5JaQU7JF7BTDDy")
  ,("eql"      ,eqlJet      ,xx "ChSVfUm3xqxJmoiGNyDSaDN9M2zQvEEZYgCL8tTUHY7V")
  ,("trk"      ,trkJet      ,xx "8ZuE9KAorFXCGehzTxVx5StfhJnYhWS4BDPSExYvSx2F")
  ,("die"      ,dieJet      ,xx "3Gojkov63gvagvD5ecU8GC8MtcStBu2N4MLm1X9sK7pA")

  ,("idx"      ,idxJet      ,xx "FSH7zD4Fboh4aFczaaQxFp7dYYvdiQBBkpnGvVM79dyq")
  ,("get"      ,getJet      ,xx "97XTM9c2fEPVXjTm2Ry4ftNke2gE4fL4VthTQj6WuHMw")
  ,("len"      ,vlenJet     ,xx "E9DR91tRJGWBUAK8Evmx25uo9G1Gadn8r1CjgEqzfxAh")
  ,("weld"     ,vweldJet    ,xx "64KonmazVYKz8PFxZHaWZ3pEQYyTApwYnmrXidPNQuez")
  ,("map"      ,vmapJet     ,xx "BVmE7N3aC9hjrG8jaFnT5GWhWgUxnnfydatXrZRzqbrs")
  ,("put"      ,vputJet     ,xx "F8UzsmUCWvCVns5vQ6dAoU7zhe74sXYR16H2fLiFS3Mv")
  ,("take"     ,vtakeJet    ,xx "4A9K4jEddmCi5m6guDukqwoG2QFjf29RDSB12GFkGmpk")
  ,("drop"     ,vdropJet    ,xx "5Gir4V8EDa9HfwGnKdPehkbom3pW91YQs5MehEs5Mv33")

  ,("barIdx"   ,bIdxJet     ,xx "HbgdQgqiXMWifLR9VViozVrsPLDfpBzJ3GP9F9ya8zz1")
  ,("barWeld"  ,bWeldJet    ,xx "HushXUNLmGA2FXGyQSU79nAZK74aSQRqwgAcgP96UmFS")
  ,("barConcat",bConcatJet  ,xx "DUbE5HYgn8nB8CypXmca5AH4xisG2pURggd4EDu59q8U")

  ,("match"    ,matchJet    ,xx "DXnz6XAngvPQTL9bWcwPjGbbvXpztKspf8CDcs2zPDy3")
  ,("switch"   ,switchJet   ,xx "4ircYrbfoUEzxhwd2XXDVV951pthnQpgFdRcfmSq4qeV")
  ,("tabMatch" ,tabMatchJet ,xx "HDh7DCABxndaF1bPGAezVnz9o7X1Sr7jXpcJQD6UnP3G")
  ,("tabSwitch",tabSwitchJet,xx "Bocjwy6qgxYkhQSWJYL2Crm45VfxeQgNkR9vxZgp4Z4i")
  ,("tabIdx"   ,tabIdxJet   ,xx "9QCviZE5sQoQFKGXU11UW4BYDCuGzVXbmkUd9Pjke7cD")
  ,("add32"    ,add32Jet    ,xx "HZ4jJ3MzmHSf4QmH1Razq2WoyangLiSMLx3CY1WGB4M9")
  ,("sub32"    ,sub32Jet    ,xx "BFrP5zH9gHEksWuHP5DHPCCoNxFcxwVcqcCAoGnG2DCZ")
  ,("mul32"    ,mul32Jet    ,xx "F86MN69Za5o6PEnEW5HQphW6kUPQSPfecifofn2Lab39")
  ,("div32"    ,div32Jet    ,xx "96dGnqxfPv32cKSnp3fqCixmM99k5qRf1G7rbF6yrcdd")
  ,("and32"    ,and32Jet    ,xx "BKEiL4joThg96QTxUeoEAzjGHj2R7Vsg32HSHp34oj5o")
  ,("xor32"    ,xor32Jet    ,xx "2D1bDBj9hhe449CyKudgBuLmurVBXM8U1m2muoL8QuVU")
  ,("or32"     ,or32Jet     ,xx "BgEFftkJCvdX9cDTBq4Dmx1JFCKUkWw1GG5SZdr9Zgdz")
  ,("lsh32"    ,lsh32Jet    ,xx "FnYGKimC3mu8antx2zJru4CHr3Uqn7vEB4mniMoqAxV8")
  ,("rsh32"    ,rsh32Jet    ,xx "2Ppq87d11gnkGzRhgrG7i9qtBiKrW3HzcUWFushxRKTv")
  ,("ror32"    ,ror32Jet    ,xx "6jsb1Ymq8RHVjqzcwr4bfcD3VKtkbayebqcRq9WbmzXk")
  ,("rol32"    ,rol32Jet    ,xx "HeaanJnwYU3wy6PuqpnxsBu5U8eHnMSFJftrwapgDDgE")

  ,("concat"   ,concatJet   ,xx "G51qSwgUzG9JhJ3H24PEB9FpSJ8mq7MVKfGHT9VhqVgu")
  ,("implode"  ,implodeJet  ,xx "A6QTDwkJDQE6rzgYpmWZTWMdwxBwaG6dRo5eVJAaQV4A")
  ]
 where
  matchJet     k xs = orExec k xs Nothing
  switchJet    k xs = orExec k xs Nothing
  tabMatchJet  k xs = orExec k xs Nothing
  tabSwitchJet k xs = orExec k xs Nothing
  tabIdxJet    k xs = orExec k xs Nothing

  concatJet k v@[x] = orExec k v do
      vs <- getRow x
      xs <- for vs getRow
      pure $ nodVal $ DAT $ ROW $ concat xs

  -- We don't accept 0 bytes, since their behavior in the plunder
  -- implementation is weird (silently dropped)
  --
  -- TODO Converting from a vector to a list to a bytestring to an atom
  -- is stupid `Natty` should be extended to support `Vector Word8`.
  implodeJet k v@[x] = orExec k v $ do
      vs <- getRow x
      bs <- for vs \case
          (VAL _ (NAT n)) | n>0 && n<256 -> Just (fromIntegral n)
          _                              -> Nothing
      pure $ AT $ Natty.bytesNat $ pack $ toList bs

  xx :: Text -> ByteString
  xx = decodeBtc

  orExec core _  (Just x) = x
  orExec core xs Nothing  = case core of
      VAL _ (LAW l) -> lawExec l (core:xs)
      _             -> foldl' (%%) core xs

  orExecTrace _ core xs res = orExec core xs res

  --  orExecTrace msg core xs res = case res of
  --      Nothing -> trace (msg <> ".nomatch") (orExec core xs res)
  --      Just{}  -> trace (msg <> ".match")   (orExec core xs res)

  -- Since we need to convert the index to an `Int`, we need to be
  -- careful about overflow (input bigger than INTMAX is possible).
  -- Thus we compare to the length manually, knowing that a vector of
  -- size>INTMAX is impossible. (TODO True on all archs?)
  vidx :: Nat -> Vector Val -> Val
  vidx ind vec =
      let siz = fromIntegral (length vec)
      in if (ind >= siz)
         then AT 0
         else vec ! fromIntegral ind

  idxJet k v@[ix,row] = orExec k v (vidx (toNat ix) <$> getRow row)
  getJet k v@[row,ix] = orExec k v (vidx (toNat ix) <$> getRow row)

  vlenJet k v@[x] = orExec k v (AT . fromIntegral . length <$> getRow x)

  -- TODO: vsplice

  vweldJet k v@[x,y] = orExec k v (vweld <$> getRow x <*> getRow y)
    where
      vweld :: Vector Val -> Vector Val -> Val
      vweld x y = VAL 1 $ DAT $ ROW (x ++ y)

  vmapJet k v@[x,y] = orExec k v (vmap x <$> getRow y)
    where
      vmap :: Val -> Vector Val -> Val
      vmap fun vec = VAL 1 $ DAT $ ROW $ fmap (fun %%) vec

  -- TODO: vfind

  vputJet k v@[x,y,z] = orExec k v (vput (toNat y) z <$> getRow x)
    where
      vput :: Nat -> Val -> Vector Val -> Val
      vput ind val vec =
        let siz = fromIntegral (length vec)
        in VAL 1 $ DAT $ ROW $ if (ind >= siz)
           then vec
           else vec // [(fromIntegral ind, val)]

  vtakeJet k v@[x,y] = orExec k v (vtake (toNat x) <$> getRow y)
    where
      vtake :: Nat -> Vector Val -> Val
      vtake n vec =
        let siz = fromIntegral (length vec)
        in VAL 1 $ DAT $ ROW $ if (n >= siz)
           then vec
           else V.take (fromIntegral n) vec

  vdropJet k v@[x,y] = orExec k v (vdrop (toNat x) <$> getRow y)
    where
      vdrop :: Nat -> Vector Val -> Val
      vdrop n vec =
        let siz = fromIntegral (length vec)
        in VAL 1 $ DAT $ ROW $ if (n >= siz)
           then V.empty
           else V.drop (fromIntegral n) vec

  -- TODO: =VCHUNKS

  getBar (VAL _ (DAT (BAR b))) = Just b
  getBar _                     = Nothing

  bIdxJet k v@[x,y] = orExec k v (bidx (toNat x) <$> getBar y)
    where
      bidx :: Nat -> ByteString -> Val
      bidx n bs =
        let siz = fromIntegral (length bs)
        in AT $ if (n >= siz)
                then 0
                else fromIntegral $ BS.index bs $ fromIntegral n

  bConcatJet :: Val -> [Val] -> Val
  bConcatJet k v@[x] =
      orExecTrace "BCONCAT" k v $ do
          vs <- getRow x
          bs <- traverse getBar vs
          pure $ mkBar $ concat bs

  bWeldJet :: Val -> [Val] -> Val
  bWeldJet k v@[x,y] = orExecTrace "BWELD" k v (bweld <$> getBar x <*> getBar y)
    where
      bweld :: ByteString -> ByteString -> Val
      bweld x y = mkBar (x <> y)

  dieJet _ [x] = unsafePerformIO $ do
      s <- readIORef vShowPlun
      o <- s x
      redOut $ ("\n" <> o <> "\n")
      throwIO (UserError x)

  trkJet _ [x,y] = unsafePerformIO $ do
      s <- readIORef vShowPlun
      o <- s x
      greenOut o
      pure y

  addJet _ [x,y] = AT (toNat x + toNat y)
  mulJet _ [x,y] = AT (toNat x * toNat y)
  subJet _ [a,b] = let (x,y) = (toNat a, toNat b)
                   in AT (if y>=x then 0 else (x-y))
  eqlJet _ [x,y] = AT $ bool 0 1 $ (x==y)
  aeqJet _ [x,y] = AT $ bool 0 1 $ (toNat x == toNat y)
  bexJet _ [x]   = AT (2 ^ toNat x)
  modJet _ [x,y] = AT (toNat x `mod` toNat y)
  divJet _ [x,y] = AT (toNat x `div` toNat y)

  w32op fun _ [x,y] = AT $ fromIntegral $ fun (w32 $ toNat x) (w32 $ toNat y)
  add32Jet = w32op (+)
  mul32Jet = w32op (*)
  div32Jet = w32op (div)
  sub32Jet = w32op (-)
  and32Jet = w32op (.&.)
  xor32Jet = w32op xor
  or32Jet  = w32op (.|.)

  w32opInt fun k [x,y] = AT $ fromIntegral
                       $ fun (w32 $ toNat x) (fromIntegral $ w32 $ toNat y)

  lsh32Jet = w32opInt shiftL
  rsh32Jet = w32opInt shiftR
  ror32Jet = w32opInt rotateR
  rol32Jet = w32opInt rotateL

jetMatch :: Pin -> Pin
jetMatch b@(P _ref _blob haz _exe core) = unsafePerformIO $ do
    let hashText = encodeBtc haz
    let name = case core of
                 VAL _ (LAW l) -> lawNameText $ lawName l
                 _             -> ""
    if not (member name allJetNames)
    then pure b
    else do
        -- yellowOut name
        case lookup (pinHash b) jetsByHash of
            Just (nm, _) | nm /= name -> do
                hPutStrLn stderr name
                hPutStrLn stderr ("\t" <> tshow hashText)
                hPutStrLn stderr ("WRONG JET MATCHED: " <> nm)
                error "aborting"
            Nothing -> do
                hPutStrLn stderr name
                hPutStrLn stderr ("\t" <> tshow hashText)
                hPutStrLn stderr "\tNOT MATCHED"
                pure b
            Just (_nm, exe) -> do
                -- yellowOut (name <> " matched")
                pure $ b { pinExec = exe core }

jetsByHash :: Map ByteString (Text, (Val -> [Val] -> Val))
jetsByHash = mapFromList (table <&> \(n,f,h) -> (h,(n,f)))

allJetNames :: Set Text
allJetNames = setFromList $ fmap (\(n,_,_) -> n) table
