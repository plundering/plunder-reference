{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Werror #-}

module Runner
    ( Inq(..)
    , execFx
    , getResponse'
    , Ship(..)
    , vShips
    , newTopShip
    , roundToBex64
    )
where

import PlunderPrelude
import Data.Primitive.SmallArray

import qualified Data.Vector as V
import qualified Plun        as P

import Control.Concurrent    (threadDelay)
import Control.Monad.State   (MonadState, evalStateT, get)
import Crypto.Sign.Ed25519   (PublicKey(..), SecretKey(..))
import Crypto.Sign.Ed25519   (createKeypair, toPublicKey)
import Data.Bits             (shiftR, (.|.))
import Data.Time.Clock       (nominalDiffTimeToSeconds)
import Data.Time.Clock.POSIX (POSIXTime, getPOSIXTime)
import Plun                  (Fan(NAT,BAR), (%%))
import Plun.Print            (encodeBtc)
import System.Entropy        (getEntropy)

--------------------------------------------------------------------------------

newtype Key = KEY { unkey :: SecretKey }

instance Show Key where
  show = show . take 8 . encodeBtc . unSecretKey . unkey

newtype Pub = PUB { unpub :: PublicKey }
  deriving newtype (Eq, Ord)

instance Show Pub where
  show = show . take 4 . encodeBtc . unPublicKey . unpub

data Inq = INQ
    { inqWhen :: TMVar (Nat, Fan)
    , inqSend :: TMVar (Nat, Fan)
    , inqRecv :: TMVar (Nat, Fan)
    , inqWait :: TMVar (Nat, Fan)
    , inqFork :: TMVar (Nat, Fan)
    , inqKGen :: TMVar (Nat, Fan)
    , inqRand :: TMVar (Nat, Fan)
    }

data Request
    = SEND Key Pub Fan
    | RECV Key (Set Pub)
    | WHEN
    | WAIT Nat -- Wen
    | FORK Fan
    | RAND
    | KGEN
 deriving (Show)

{-
    - `shipName` is an internal name for debugging purposes.

    - `paterName` is the internal name of the ship that spawned this one.

    - `shipNumReqs` is the number of requests after the last event.
       It is generally smaller than `shipRequests` for performance reasons
       (to avoid resizing).
-}
data Ship = SHIP
    { shipShipName  :: !Text
    , shipPaterName :: !(Maybe Text)
    , shipNextSonId :: !Nat
    , shipInputQs   :: !Inq
    , shipNumReqs   :: !Int
    , shipRequests  :: !(SmallMutableArray RealWorld (Fan, Maybe (Async ())))
    , shipNoun      :: !Fan
    }
  deriving (Generic)

type MSend = (Pub, Fan, MVar ())

type KingState = Map Text (Inq, Async Void)

vShips :: TVar KingState
vShips = unsafePerformIO (newTVarIO mempty)

--------------------------------------------------------------------------------

makeFieldLabels ''Ship

--------------------------------------------------------------------------------

nanosSinceEpoch :: POSIXTime -> Nat
nanosSinceEpoch = floor . (1e9 *) . nominalDiffTimeToSeconds

getNat :: Fan -> Maybe Nat
getNat (NAT n) = Just n
getNat _       = Nothing

getBlob :: Fan -> Maybe ByteString
getBlob (P.BAR n) = Just n
getBlob _         = Nothing

getPublicKey :: Fan -> Maybe Pub
getPublicKey v = do
    b <- getBlob v
    guard (length b == 32)
    pure (PUB $ PublicKey b)

getSecretKey :: Fan -> Maybe Key
getSecretKey v = do
    b <- getBlob v
    guard (length b == 64)
    pure (KEY $ SecretKey b)

getCell :: Fan -> Maybe (Fan, Fan)
getCell v@P.KLO{} = Just (P.boom v)
getCell _         = Nothing

getRequests :: Fan -> Maybe (Vector Fan)
getRequests = getCell >=> (P.getRow . snd)

getReq :: Fan -> Maybe Request
getReq n = do
    (toList <$> P.getRow n) >>= \case
        [NAT 1852139639]         -> pure WHEN
        [NAT 1953063287, timeN]  -> WAIT <$> getNat timeN
        [NAT 1802661734, sparkN] -> pure (FORK sparkN)
        [NAT 1852139371]         -> pure KGEN
        [NAT 1684955506]         -> pure RAND

        [NAT 1684956531, skeyN, addrN, bodyN] -> do
            skey <- getSecretKey skeyN
            addr <- getPublicKey addrN
            pure (SEND skey addr bodyN)

        (NAT 1986225522 : skeyN : shipNs) -> do
            skey <- getSecretKey skeyN
            recv <- traverse getPublicKey shipNs
            pure (RECV skey (setFromList recv))

        _ -> do
            Nothing

roundToBex64 :: Word64 -> Word64
roundToBex64 v =
    g+1
  where
    a = v-1
    b = a .|. (shiftR a 1)
    c = b .|. (shiftR b 2)
    d = c .|. (shiftR c 4)
    e = d .|. (shiftR d 8)
    f = e .|. (shiftR e 16)
    g = f .|. (shiftR f 32)

growRequestsTable :: Int -> (MonadIO m, MonadState Ship m) => m ()
growRequestsTable requestedSize = do
    SHIP{..} <- get
    let size = sizeofSmallMutableArray shipRequests
    when (size < requestedSize) do
        let newSize = fromIntegral $ roundToBex64 $ fromIntegral requestedSize
        grownTable <- liftIO $ newSmallArray newSize (NAT 0, Nothing)
        liftIO (copySmallMutableArray grownTable 0 shipRequests 0 size)
        assign #requests grownTable

execFx :: (MonadIO m, MonadState Ship m) => m ()
execFx = do
    newReqs <- fromMaybe mempty . getRequests . shipNoun <$> get
    let newNumReqs = length newReqs

    growRequestsTable newNumReqs
    oldReqs    <- use #requests
    oldNumReqs <- use #shipNumReqs

    assign #shipNumReqs newNumReqs

    V.iforM_ newReqs \i req -> do
        (exr, mTid) <- liftIO (readSmallArray oldReqs i)
        unless (exr == req) $ do
            maybe (pure()) cancel mTid
            unsafeCreateReq i req -- This will overwrite

    liftIO $ do
        for_ [newNumReqs .. (oldNumReqs-1)] \i -> do
            unsafeCancelReq i oldReqs

unsafeCancelReq
    :: Int
    -> SmallMutableArray RealWorld (Fan, Maybe (Async ()))
    -> IO ()
unsafeCancelReq key tab = do
    readSmallArray tab key >>= \case
        (_, Nothing)  -> pure ()
        (_, Just tid) -> cancel tid
    writeSmallArray tab key (NAT 0, Nothing)

newTopShip :: Text -> Maybe Text -> Fan -> IO (Inq, Ship)
newTopShip name mPater val = do
    q <- atomically $ INQ <$> newEmptyTMVar
                          <*> newEmptyTMVar
                          <*> newEmptyTMVar
                          <*> newEmptyTMVar
                          <*> newEmptyTMVar
                          <*> newEmptyTMVar
                          <*> newEmptyTMVar

    reqs <- newSmallArray 0 (NAT 0, Nothing)

    pure $ (q,) $ SHIP { shipShipName  = name
                       , shipPaterName = mPater
                       , shipNextSonId = 1
                       , shipInputQs   = q
                       , shipRequests  = reqs
                       , shipNumReqs   = 0
                       , shipNoun      = val
                       }


launchShip :: MonadIO m => Text -> Text -> Fan -> m ()
launchShip pater name val = liftIO $ do
    (q, initVal) <- newTopShip name (Just pater) val

    tid :: Async Void <- async $ evalStateT execShip initVal

    atomically $ modifyTVar vShips
               $ insertMap name (q, tid)

-- TODO Make sure this in priority order, and not "fair"
getResponse :: MonadIO m => Inq -> m (Bool, (Nat, Fan))
getResponse INQ{..} =
    liftIO $ atomically $ asum
        [ (True,)  <$> takeTMVar inqWhen -- current time
        , (True,)  <$> takeTMVar inqFork -- fork ack
        , (True,)  <$> takeTMVar inqSend -- send ack
        , (True,)  <$> takeTMVar inqWait -- timer
        , (True,)  <$> takeTMVar inqKGen -- generated key
        , (True,)  <$> takeTMVar inqRand -- random entropy
        , (False,) <$> takeTMVar inqRecv -- input message
        ]

getResponse' :: (MonadIO m, MonadState Ship m) => m (Fan, Fan)
getResponse' = do
    inq <- use #inputQs
    (oneShot, (reqId, respVal)) <- getResponse inq
    when oneShot do
        let key = fromIntegral reqId
        -- TODO Probably don't actually need to cancel, it should be done.
        reqs <- use (to shipRequests)
        liftIO $ when (key < sizeofSmallMutableArray reqs)
               $ unsafeCancelReq key reqs
    pure (NAT reqId, respVal)

execShip :: (MonadIO m, MonadState Ship m) => m a
execShip = do
    execFx
    (rid, respVal) <- getResponse'
    !st <- (\s -> force (s %% rid %% respVal)) <$> use #noun
    assign #noun st
    execShip

nextSonId :: (MonadIO m, MonadState Ship m) => m Nat
nextSonId = do
    next <- use #nextSonId
    modifying #nextSonId succ
    pure next

withKeyPort :: Pub -> Key -> Set Pub
            -> (IO MSend -> IO Void)
            -> IO ()
withKeyPort adr _key wl act = do
    var <- newEmptyMVar
    idx <- atomically $ do
        ports <- readTVar vPorts
        case lookup adr ports of
            Nothing -> do
                let nex = 1
                let tab = insertMap 0 (wl,var) mempty
                writeTVar vPorts (insertMap adr (nex,tab) ports)
                pure 0
            Just (nex, tab) -> do
                let tab' = insertMap nex (wl,var) tab
                writeTVar vPorts (insertMap adr (nex+1, tab') ports)
                pure nex

    let cleanup :: IO ()
        cleanup = atomically do
            ports <- readTVar vPorts
            case lookup adr ports of
                Nothing    -> pure ()
                Just (n,t) -> do
                    let t' = deleteMap idx t
                    writeTVar vPorts (insertMap adr (n,t') ports)

    -- Insert a listener into some data structure.
    -- Register a cleanup action to remove it.
    -- Define a procedure that reads from the listener.
    -- Run the callback on the procedure.
    finally (fmap absurd $ act $ takeMVar var) cleanup

vPorts :: TVar (Map Pub (Int, IntMap (Set Pub, MVar MSend)))
vPorts = unsafePerformIO (newTVarIO mempty)

getOpenPort :: Pub -> Pub -> IO (MVar MSend)
getOpenPort src dst = do
    ports <- atomically (readTVar vPorts)

    let getMatch :: [(Set Pub, MVar MSend)]
                 -> Maybe (MVar MSend)
        getMatch []     = Nothing
        getMatch ((wl,v):_)  | null wl       = Just v
        getMatch ((wl,v):_)  | member src wl = Just v
        getMatch (_:ms)                      = getMatch ms

    let matchers = toList $ fromMaybe mempty $ fmap snd $ lookup dst ports

    case getMatch matchers of
        Nothing -> do threadDelay 100_000
                      getOpenPort src dst
        Just mv -> pure mv

pshow :: Fan -> Text
pshow v = unsafePerformIO $ do
    f <- readIORef P.vShowPlun
    f v

{-
    Unsafe because it assumes that the requests table is large enough
    to write the new request into.

    This is only called from `execFx`, and that grows the requests table
    before doing anything, so this should work.
-}
unsafeCreateReq :: (MonadIO m, MonadState Ship m) => Int -> Fan -> m ()
unsafeCreateReq key reqVal = do
    que <- use #inputQs
    snm <- use #shipName

    let respond whichQ resp =
            atomically $ putTMVar (whichQ que)
                       $ (fromIntegral key, resp)

    reqs <- use (to shipRequests)

    let ins act = liftIO $ do
            tid <- async act
            writeSmallArray reqs key (reqVal, Just tid)

    let mReq = getReq reqVal

    let putStrsLn ss = putStrLn (concat ss)
    putStrsLn $ case mReq of
        Nothing -> [ snm, "[", tshow key, "]=bad(", pshow reqVal, ")" ]
        Just rq -> [ snm, "[", tshow key, "]=(",    tshow rq,     ")" ]

    case mReq of
        -- Even though this isn't a valid request we still record
        -- the value so that we get correct request diffs.
        Nothing -> liftIO $ do
            writeSmallArray reqs key (reqVal, Nothing)
        Just (SEND srcKey dst msg) -> ins do
            let src = PUB $ toPublicKey $ unkey srcKey
            mAck <- newEmptyMVar
            port <- getOpenPort src dst
            ()   <- putMVar port (src, msg, mAck)
            takeMVar mAck
            respond inqSend (NAT 0)
        Just (RECV dstKey wl) -> ins do
            let port = PUB $ toPublicKey $ unkey dstKey
            let loop portRecv = do
                    (src, msg, ack) <- portRecv
                    let srcN = BAR $ unPublicKey $ unpub src
                    respond inqRecv (P.mkRow [srcN, msg])
                    putMVar ack () -- TODO Somehow do this when we process
                                   -- the event instead?
                    loop portRecv
            withKeyPort port dstKey wl loop
        Just (WAIT later) -> ins do
            now <- nanosSinceEpoch <$> getPOSIXTime
            when (now < later) $ do
                -- TODO Handle values larger than MAXINT
                threadDelay (fromIntegral ((later - now) `div` 1000))
            respond inqWhen (NAT 0)
        Just (FORK st) -> do
            idn <- nextSonId
            nam <- use #shipName
            ins do
                launchShip nam (nam <> "-" <> tshow idn) st
                respond inqFork (NAT 0)
        Just RAND -> ins do
            bar <- getEntropy 32
            respond inqRand (BAR bar)
        Just WHEN -> ins do
            now <- NAT . nanosSinceEpoch <$> getPOSIXTime
            respond inqWhen now
        Just KGEN -> ins do
            (pub, secret) <- createKeypair
            let keyN = BAR (unSecretKey secret)
            let pubN = BAR (unPublicKey pub)
            let resp = P.mkRow [pubN, keyN]
            respond inqKGen resp
