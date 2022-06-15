module Server.Convert where

import PlunderPrelude

import Numeric.Natural

import Plun
import Server.Types.Logging

import qualified Data.Map    as M
import qualified Data.Vector as V

-- Quick, crappy conversion between nouns and server data structures.

class ToNoun a where
  toNoun :: a -> Val

class FromNoun a where
  fromNoun :: Val -> Maybe a

instance ToNoun Natural where
  toNoun n = AT $ n
instance FromNoun Natural where
  fromNoun (AT n) = Just n
  fromNoun _      = Nothing

instance ToNoun ByteString where
  toNoun = mkBar
instance FromNoun ByteString where
  fromNoun (VAL _ (DAT (BAR n))) = Just n
  fromNoun  _                    = Nothing

instance ToNoun String where
  toNoun = AT . utf8Nat . pack
instance FromNoun String where
  fromNoun (AT n) = case (natUtf8 n) of
    Left _  -> Nothing
    Right t -> Just $ unpack t
  fromNoun _ = Nothing

getRawRow :: Val -> Maybe (Vector Val)
getRawRow (VAL _ (DAT (ROW xs))) = Just xs
getRawRow _                      = Nothing

getRawTable :: Val -> Maybe (Map Nat Val)
getRawTable (VAL _ (DAT (TAB m))) = Just m
getRawTable _                     = Nothing

instance (ToNoun a) => ToNoun (Vector a) where
  toNoun = VAL 1 . DAT . ROW . (fmap toNoun)

instance (FromNoun a) => FromNoun (Vector a) where
  fromNoun n = getRawRow n >>= mapM fromNoun

instance (ToNoun a) => ToNoun (Maybe a) where
  toNoun Nothing  = AT 0
  toNoun (Just a) = mkRow [AT 1, toNoun a]

instance (FromNoun a) => FromNoun (Maybe a) where
  fromNoun (AT 0) = (Just Nothing)
  fromNoun n = do
    r <- getRawRow n
    r0 <- r V.!? 0
    guard (r0 == (AT 1))
    r1 <- r V.!? 1
    Just <$> fromNoun r1


instance ToNoun MachineName where
  toNoun (MachineName str) = toNoun str
instance FromNoun MachineName where
  fromNoun n = MachineName <$> fromNoun n

instance ToNoun ThreadId where
  toNoun (ThreadId i) = toNoun i
instance FromNoun ThreadId where
  fromNoun n = ThreadId <$> fromNoun n

instance ToNoun RequestIdx where
  toNoun (RequestIdx i) = AT $ fromIntegral i
instance FromNoun RequestIdx where
  fromNoun n = (RequestIdx . fromIntegral) <$> fromNoun @Nat n

instance ToNoun Snapshot where
  toNoun (Snapshot m) = VAL 1 $ DAT $ TAB $ M.mapKeys unThreadId m
instance FromNoun Snapshot where
  fromNoun n = do
    tbl <- getRawTable n
    pure $ Snapshot $ M.mapKeys ThreadId tbl

instance ToNoun BatchNum where
  toNoun (BatchNum n) = toNoun n
instance FromNoun BatchNum where
  fromNoun n = BatchNum <$> fromNoun n

instance ToNoun Receipt where
  toNoun ReceiptInit{..} = mkRow [AT 0, toNoun initTid, initVal]
  toNoun ReceiptFork{..} =
    mkRow [AT 1, toNoun forkReqTid, toNoun forkReqIdx, toNoun forkAssignedTid]
  toNoun ReceiptVal{..} =
    mkRow [AT 2, toNoun receiptTid, toNoun receiptIdx, receiptVal]
  toNoun ReceiptRecv{..} =
    mkRow [AT 3, toNoun recvTid, toNoun recvIdx, toNoun recvSendTid,
           toNoun recvSendIdx]
  toNoun ReceiptKill{..} =
    mkRow [AT 4, toNoun killTidNotified, toNoun killIdx]

parseAt :: FromNoun a => Vector Val -> Int -> Maybe a
parseAt v i = v V.!? i >>= fromNoun

getRaw :: Vector Val -> Int -> Maybe Val
getRaw = (V.!?)

instance FromNoun Receipt where
  fromNoun n = do
    r <- getRawRow n
    let len = V.length r
    t <- r V.!? 0 >>= fromNoun @Nat
    case (t, len) of
      (0, 3) -> ReceiptInit <$> parseAt r 1 <*> getRaw r 2
      (1, 4) -> ReceiptFork <$> parseAt r 1 <*> parseAt r 2 <*> parseAt r 3
      (2, 4) -> ReceiptVal <$> parseAt r 1 <*> parseAt r 2 <*> getRaw r 3
      (3, 5) -> ReceiptRecv <$> parseAt r 1 <*> parseAt r 2 <*> parseAt r 3
                            <*> parseAt r 4
      (4, 3) -> ReceiptKill <$> parseAt r 1 <*> parseAt r 2
      _ -> Nothing

instance ToNoun LogBatch where
  toNoun LogBatch{..} = mkRow [
    toNoun batchNum,
    toNoun writeTime,
    toNoun lastSnapshot,
    toNoun snapshot,
    toNoun $ V.fromList executed
    ]

instance FromNoun LogBatch where
  fromNoun n = do
    r <- getRawRow n
    batchNum <- r V.!? 0 >>= fromNoun
    writeTime <- r V.!? 1 >>= fromNoun
    lastSnapshot <- r V.!? 2 >>= fromNoun
    snapshot <- r V.!? 3 >>= fromNoun
    executed <- r V.!? 4 >>= fromNoun <&> V.toList
    pure LogBatch{..}
