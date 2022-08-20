-- | Quick, crappy conversion between nouns and server data structures.
module Server.Convert where

import PlunderPrelude

import Numeric.Natural

import Plun
import Server.Types.Logging
                            -- to this module?

import Data.Vector ((!))

import qualified Data.Map    as M
import qualified Data.Vector as V

class ToNoun a where
  toNoun :: a -> Fan

class FromNoun a where
  fromNoun :: Fan -> Maybe a

instance ToNoun Bool where
  toNoun True  = NAT 1
  toNoun False = NAT 0
instance FromNoun Bool where
  fromNoun (NAT 0) = Just False
  fromNoun (NAT 1) = Just True
  fromNoun _      = Nothing

instance ToNoun Fan where
  toNoun = id
instance FromNoun Fan where
  fromNoun = Just . id

instance ToNoun Natural where
  toNoun n = NAT n
instance FromNoun Natural where
  fromNoun (NAT n) = Just n
  fromNoun _      = Nothing

instance (ToNoun a,ToNoun b) => ToNoun (a,b)
  where toNoun(x,y) = mkRow[toNoun x, toNoun y]

instance (ToNoun a,ToNoun b,ToNoun c) => ToNoun (a,b,c)
  where toNoun(x,y,z) = mkRow[toNoun x, toNoun y, toNoun z]

instance (ToNoun a,ToNoun b,ToNoun c,ToNoun d) => ToNoun (a,b,c,d)
  where toNoun(p,q,r,s) = mkRow[toNoun p,toNoun q,toNoun r,toNoun s]

instance (ToNoun a,ToNoun b,ToNoun c,ToNoun d,ToNoun e) => ToNoun (a,b,c,d,e)
  where toNoun(p,q,r,s,t) = mkRow[toNoun p,toNoun q,toNoun r,toNoun s,toNoun t]

instance (FromNoun a,FromNoun b)
    => FromNoun (a,b)
  where
    fromNoun n = do
      r <- getRawRow n
      guard (length r == 2)
      (,) <$> fromNoun (r!0)
          <*> fromNoun (r!1)

instance (FromNoun a,FromNoun b,FromNoun c,FromNoun d,FromNoun e)
    => FromNoun (a,b,c,d,e)
  where
    fromNoun n = do
      r <- getRawRow n
      guard (length r == 5)
      (,,,,) <$> fromNoun (r!0)
             <*> fromNoun (r!1)
             <*> fromNoun (r!2)
             <*> fromNoun (r!3)
             <*> fromNoun (r!4)

instance ToNoun ByteString where
  toNoun = BAR
instance FromNoun ByteString where
  fromNoun (BAR n) = Just n
  fromNoun  _      = Nothing

instance ToNoun Text where
  toNoun = NAT . utf8Nat
instance FromNoun Text where
  fromNoun (NAT n) = case (natUtf8 n) of
    Left _  -> Nothing
    Right t -> Just t
  fromNoun _ = Nothing

getRawRow :: Fan -> Maybe (Vector Fan)
getRawRow (ROW xs) = Just xs
getRawRow _        = Nothing

getRawTable :: Fan -> Maybe (Map Nat Fan)
getRawTable (TAB m) = Just m
getRawTable _       = Nothing

instance ToNoun a => ToNoun (Vector a) where
  toNoun = ROW . (fmap toNoun)
instance FromNoun a => FromNoun (Vector a) where
  fromNoun n = getRawRow n >>= mapM fromNoun

-- | Since we are very unlikely to ever want actual noun linked-lists
-- at an API boundary, we represent lists as rows.
instance ToNoun a => ToNoun [a] where
  toNoun = toNoun . V.fromList

-- | Since we are very unlikely to ever want actual noun linked-lists
-- at an API boundary, we represent lists as rows.
instance FromNoun a => FromNoun [a] where
  fromNoun n = toList @(Vector a) <$> fromNoun n

instance ToNoun a => ToNoun (Map Nat a) where
  toNoun = TAB . (map toNoun)

instance FromNoun a => FromNoun (Map Nat a) where
  fromNoun n = getRawTable n >>= mapM fromNoun

-- | TODO Benj: I've been using 0+(0 x) for these (no Row)
instance (ToNoun a) => ToNoun (Maybe a) where
  toNoun Nothing  = NAT 0
  toNoun (Just a) = mkRow [NAT 1, toNoun a]

-- | TODO Benj: I've been using 0+(0 x) for these (no Row)
instance (FromNoun a) => FromNoun (Maybe a) where
  fromNoun (NAT 0) = Just Nothing
  fromNoun n = do
    r <- getRawRow n
    r0 <- r V.!? 0
    guard (r0 == NAT 1)
    r1 <- r V.!? 1
    Just <$> fromNoun r1

instance ToNoun MachineName where
  toNoun (MachineName str) = toNoun str
instance FromNoun MachineName where
  fromNoun n = MachineName <$> fromNoun n

instance ToNoun ProcessId where
  toNoun (ProcessId i) = NAT $ fromIntegral i
instance FromNoun ProcessId where
  fromNoun n = (ProcessId . fromIntegral) <$> fromNoun @Nat n

instance ToNoun RequestId where
  toNoun (RequestId i) = NAT $ fromIntegral i
instance FromNoun RequestId where
  fromNoun n = (RequestId . fromIntegral) <$> fromNoun @Nat n

instance ToNoun Snapshot where
  toNoun (Snapshot v) = toNoun v
instance FromNoun Snapshot where
  fromNoun v = Snapshot <$> fromNoun v

instance ToNoun BatchNum where
  toNoun (BatchNum n) = toNoun n
instance FromNoun BatchNum where
  fromNoun n = BatchNum <$> fromNoun n

instance ToNoun Receipt where
   toNoun = \case
       ReceiptInit{..} -> t (NAT 0, initVal, initPid)
       ReceiptFork{..} -> t (NAT 1, forkReqPid, forkReqIdx, forkAssignedPid)
       ReceiptVal{..}  -> t (NAT 2, receiptPid, receiptIdx, receiptVal)
       ReceiptRecv{..} -> t (NAT 3, recvPid, recvIdx, recvSendPid, recvSendIdx)
     where
       t :: ToNoun a => a -> Fan
       t = toNoun

parseAt :: FromNoun a => Vector Fan -> Int -> Maybe a
parseAt v i = v V.!? i >>= fromNoun

getRaw :: Vector Fan -> Int -> Maybe Fan
getRaw = (V.!?)

instance FromNoun Receipt where
  fromNoun n = do
    r <- getRawRow n
    let len = V.length r
    t <- r V.!? 0 >>= fromNoun @Nat
    case (t, len) of
      (0, 3) -> ReceiptInit <$> parseAt r 1 <*> parseAt r 2
      (1, 4) -> ReceiptFork <$> parseAt r 1 <*> parseAt r 2 <*> parseAt r 3
      (2, 4) -> ReceiptVal <$> parseAt r 1 <*> parseAt r 2 <*> getRaw r 3
      (3, 5) -> ReceiptRecv <$> parseAt r 1 <*> parseAt r 2 <*> parseAt r 3
                            <*> parseAt r 4
      _ -> Nothing

instance ToNoun LogBatch where
    toNoun LogBatch{..} =
        toNoun (batchNum, writeTime, lastSnapshot, snapshot, executed)

instance FromNoun LogBatch where
    fromNoun n = do
        (batchNum,writeTime,lastSnapshot,snapshot,executed) <- fromNoun n
        pure LogBatch{..}
