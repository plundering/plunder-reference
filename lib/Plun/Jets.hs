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

getRow :: Pln -> Maybe (Vector Pln)
getRow (PLN _ (DAT (ROW xs))) = Just xs
getRow _                      = Nothing

getByte :: Pln -> Maybe Word8
getByte (AT n) | n<256 = Just (fromIntegral n)
getByte _              = Nothing

-- Application overwrites this on startup.
vShowPlun :: IORef (Pln -> IO Text)
vShowPlun = unsafePerformIO $ newIORef $ \v -> pure "[PLUN]"

-- w32 helpers
bex32 :: Nat
bex32 = 2 ^ (32::Nat)

_w32max :: Nat
_w32max = bex32 - 1

w32 :: Nat -> Word32
w32 x = fromIntegral (x `mod` bex32)

data UserError = UserError Pln
  deriving (Exception)

instance Show UserError where
  show (UserError e) = unsafePerformIO $ do
    s <- readIORef vShowPlun
    o <- s e
    pure $ "DIE called with:\n\n" <> (unpack $ dent "   " o)

table :: [(Text, (Pln -> [Pln] -> Pln), ByteString)]
table =
  [("add"      ,addJet      ,xx "DmQSboknWfMEi5VSrqzM5AkXNcmEJAagtGum48t9D5yY")
  ,("sub"      ,subJet      ,xx "B6MvfHoH5QAQRCkFB3aF2GkmWZY5FssjmKV4VmDyEHjY")
  ,("mul"      ,mulJet      ,xx "DLZ7asf6Z8JKVJh7XYdPE2vvchwLi8SYDqg1KdmRPGWM")
  ,("aeq"      ,aeqJet      ,xx "9N5XGPQqq1iTF96ff55vLbiFrPdyG44midJ7rPABWtXK")
  ,("bex"      ,bexJet      ,xx "3r98Xw8DcPxaa3fG5UVGPbPN7Bz4ZDYXQ98d1KcNLzhM")
  ,("mod"      ,modJet      ,xx "27HkFF4zJb7KLPk8D9GcVm8PT5rF7LptAQi2kqEgsYLv")
  ,("div"      ,divJet      ,xx "9EFxnWToC1kFwPXtgj5S7F4VEpUJtaxgk8WJN5hsRLpA")
  ,("eql"      ,eqlJet      ,xx "D9HHouzoaTBYUvUetj2rqVYTwKX2GNRqbDhZ7LB4Uush")
  ,("trk"      ,trkJet      ,xx "DCwrK91sjJddiSyrP7ZMy6Lyu6rpf2tYqf7eqsLcfMhs")
  ,("die"      ,dieJet      ,xx "3Gojkov63gvagvD5ecU8GC8MtcStBu2N4MLm1X9sK7pA")

  ,("idx"      ,idxJet      ,xx "FW8n6pBV48Z8hhQcW2niWDTKkpvYeNRbuEJF9JjU2wUF")
  ,("get"      ,getJet      ,xx "EHUibM4eUSGd6uRXkRGmKGhKpQcPZQ8RX6pdx6q3VWZQ")
  ,("len"      ,vlenJet     ,xx "BjBc5TwfPCLKaQwx9PtXAaUUQR1uVwptNeF46YYZVsXA")
  ,("weld"     ,vweldJet    ,xx "Grnt1Kea92evqAJhXLm6K4u3zzEZY1KgGoAinxg5Zx3J")
  ,("map"      ,vmapJet     ,xx "2vuRXERRgMKjjHfGbFqdqorj2awf7MNTTThh9Yz8TqYW")
  ,("put"      ,vputJet     ,xx "FSxWmboBQnfET7cES8r5KTUxq2yL69VDYvhG6YhufXfG")
  ,("take"     ,vtakeJet    ,xx "Cndo1nzYGmPzM7ZKZySXEDYgydHTQ2aK6qYc7ZsRxn9k")
  ,("drop"     ,vdropJet    ,xx "AHNJ9xXbuU3s2HaGTCBra9h92S1Q3QCU6t2Y5D5xWNvh")

  ,("barIdx"   ,bIdxJet     ,xx "LRbg5M7rhKNm24TUjVG4Y1WPz48ADEgT81UqeYjvH39" )
  ,("barWeld"  ,bWeldJet    ,xx "ABTFDM1qcfm6M5Jmg4ZZZ5bMwPZ5dP2MtSQ1Di3F4g5B")
  ,("barConcat",bConcatJet  ,xx "51QytyeBfVoqhNFzfE73VJRSdFcR149nzt9QPQKph3DK")

  ,("add32"    ,add32Jet    ,xx "6Au2oaZicALF5nF7cDP1bz5S2CcYuj6nVPjx9wogZ5gs")
  ,("sub32"    ,sub32Jet    ,xx "9KPbrUn1hX18LiVGTpVChgizXyCQ27NSJrG4CEeri9nC")
  ,("mul32"    ,mul32Jet    ,xx "7jLYXUcQL1zaT5Y46eMH67UnanMGta6w6cebhpbofdgZ")
  ,("div32"    ,div32Jet    ,xx "6ySiS6RwDcoap2yWjJbrzXiamKBzg7MbUKjCZ9nNpptH")
  ,("and32"    ,and32Jet    ,xx "8KyvRkbfS9xLkwexzmejmtm4hY4uwD2weTKPCkvEYRrj")
  ,("xor32"    ,xor32Jet    ,xx "9STJngUoe39edCS4FnzrsbwfHxmhNF3xkEdjSQyrNoVg")
  ,("or32"     ,or32Jet     ,xx "C5Xa6HiQ4hCBeBZB4qEeaj1s8ve17j3d243YYbtxxPsK")
  ,("lsh32"    ,lsh32Jet    ,xx "87C1HgzwCfapDvg5ur96h7xbMszTGprrM4VYTzCe2dnJ")
  ,("rsh32"    ,rsh32Jet    ,xx "D7W5buhQactB92iqVMZV1xrLfSXvsFU5yBRs2VmG1dSo")
  ,("ror32"    ,ror32Jet    ,xx "7yLzVGehwXUdNDCnYkwaExnSAdyJbeBUHzW4TcPzdZRZ")
  ,("rol32"    ,rol32Jet    ,xx "ArL87U2iS2AEPZHkfa7mLtMkRiH9B4xTVFxieSjU3ouX")

  ,("concat"   ,concatJet   ,xx "12cuobpCrUHuWs9SKc8UCMjN88bcw5fqRep5wTNeaBMK")
  ,("implode"  ,implodeJet  ,xx "EZ9cK7jLwuTriH9wFRrFU6LfTvsfxk6SSrs5a1jgy2k" )
  ]
 where
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
          (PLN _ (NAT n)) | n>0 && n<256 -> Just (fromIntegral n)
          _                              -> Nothing
      pure $ AT $ Natty.bytesNat $ pack $ toList bs

  xx :: Text -> ByteString
  xx = decodeBtc

  orExec core _  (Just x) = x
  orExec core xs Nothing  = case core of
      PLN _ (FUN l) -> lawExec l (core:xs)
      _             -> foldl' (%%) core xs

  orExecTrace _ core xs res = orExec core xs res

  --  orExecTrace msg core xs res = case res of
  --      Nothing -> trace (msg <> ".nomatch") (orExec core xs res)
  --      Just{}  -> trace (msg <> ".match")   (orExec core xs res)

  -- Since we need to convert the index to an `Int`, we need to be
  -- careful about overflow (input bigger than INTMAX is possible).
  -- Thus we compare to the length manually, knowing that a vector of
  -- size>INTMAX is impossible. (TODO True on all archs?)
  vidx :: Nat -> Vector Pln -> Pln
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
      vweld :: Vector Pln -> Vector Pln -> Pln
      vweld x y = PLN 1 $ DAT $ ROW (x ++ y)

  vmapJet k v@[x,y] = orExec k v (vmap x <$> getRow y)
    where
      vmap :: Pln -> Vector Pln -> Pln
      vmap fun vec = PLN 1 $ DAT $ ROW $ fmap (fun %%) vec

  -- TODO: vfind

  vputJet k v@[x,y,z] = orExec k v (vput (toNat y) z <$> getRow x)
    where
      vput :: Nat -> Pln -> Vector Pln -> Pln
      vput ind val vec =
        let siz = fromIntegral (length vec)
        in PLN 1 $ DAT $ ROW $ if (ind >= siz)
           then vec
           else vec // [(fromIntegral ind, val)]

  vtakeJet k v@[x,y] = orExec k v (vtake (toNat x) <$> getRow y)
    where
      vtake :: Nat -> Vector Pln -> Pln
      vtake n vec =
        let siz = fromIntegral (length vec)
        in PLN 1 $ DAT $ ROW $ if (n >= siz)
           then vec
           else V.take (fromIntegral n) vec

  vdropJet k v@[x,y] = orExec k v (vdrop (toNat x) <$> getRow y)
    where
      vdrop :: Nat -> Vector Pln -> Pln
      vdrop n vec =
        let siz = fromIntegral (length vec)
        in PLN 1 $ DAT $ ROW $ if (n >= siz)
           then V.empty
           else V.drop (fromIntegral n) vec

  -- TODO: =VCHUNKS

  getBar (PLN _ (DAT (BAR b))) = Just b
  getBar _                     = Nothing

  bIdxJet k v@[x,y] = orExec k v (bidx (toNat x) <$> getBar y)
    where
      bidx :: Nat -> ByteString -> Pln
      bidx n bs =
        let siz = fromIntegral (length bs)
        in AT $ if (n >= siz)
                then 0
                else fromIntegral $ BS.index bs $ fromIntegral n

  bConcatJet :: Pln -> [Pln] -> Pln
  bConcatJet k v@[x] =
      orExecTrace "BCONCAT" k v $ do
          vs <- getRow x
          bs <- traverse getBar vs
          pure $ mkBar $ concat bs

  bWeldJet :: Pln -> [Pln] -> Pln
  bWeldJet k v@[x,y] = orExecTrace "BWELD" k v (bweld <$> getBar x <*> getBar y)
    where
      bweld :: ByteString -> ByteString -> Pln
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
                 PLN _ (FUN l) -> lawNameText $ lawName l
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

jetsByHash :: Map ByteString (Text, (Pln -> [Pln] -> Pln))
jetsByHash = mapFromList (table <&> \(n,f,h) -> (h,(n,f)))

allJetNames :: Set Text
allJetNames = setFromList $ fmap (\(n,_,_) -> n) table
