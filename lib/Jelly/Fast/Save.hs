{-# OPTIONS_GHC -Wall        #-}
{-# OPTIONS_GHC -Werror      #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{-# LANGUAGE NoFieldSelectors #-}

module Jelly.Fast.Save
    ( save
    , hash
    , hash'
    , Hash256
    , withContext
    )
where

import Control.Monad.Primitive (touch)
import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable
import Natty
import PlunderPrelude          hiding (hash, (%))

import Jelly.Reference (Node(..))
import Jelly.Types     (Hash256, hashToByteString)

import qualified Data.ByteString.Internal as BS
import qualified Jelly.Fast.FFI           as FFI


--------------------------------------------------------------------------------

{-# INLINE save #-}
save :: ∀a . FFI.Ctx -> Node a -> IO (Vector a, ByteString, ByteString)
save !ctx !top = do
    vPins <- newIORef @IO @[a] []
    vTemp <- newIORef @IO @[ByteString]   []

    loop vPins vTemp top

    ()  <- FFI.c_done ctx

    hed_wid <- FFI.c_head_size ctx
    hed_ptr <- mallocForeignPtrBytes (fromIntegral hed_wid)
    withForeignPtr hed_ptr \buf -> do
        _ <- BS.memset buf 0 hed_wid -- TODO: Why does this return a pointer?
        FFI.c_save_head ctx hed_wid buf

    bod_wid <- FFI.c_body_size ctx
    bod_ptr <- mallocForeignPtrBytes (fromIntegral bod_wid)
    withForeignPtr bod_ptr \buf -> do
        _ <- BS.memset buf 0 bod_wid -- TODO: Why does this return a pointer?
        FFI.c_save_body ctx bod_wid buf


    -- Make sure none of the temporary bytestrings are freed until
    -- the C code is done with them.
    FFI.c_wipe ctx
    readIORef vTemp >>= touch

    pinList <- fromList . reverse <$> readIORef vPins
    let hed = BS.BS hed_ptr (fromIntegral hed_wid)
    let bod = BS.BS bod_ptr (fromIntegral bod_wid)
    pure (pinList, hed, bod)
  where
    loop :: IORef [a] -> IORef [ByteString] -> Node a -> IO FFI.CNode
    loop !vPins !vTemp = \case
        WORD w -> do
            FFI.c_word ctx w

        NAT n -> do
            let bs@(BS.BS fpt wid) = natBytes n
            modifyIORef' vTemp (bs:)
            withForeignPtr fpt \buf -> do
                FFI.c_nat ctx (fromIntegral wid) buf

        PIN cpin h -> do
            let bs@(BS.BS fpt _) = hashToByteString h
            modifyIORef' vTemp (bs:)
            alloca \vIsUnique -> do
                poke vIsUnique (CBool 0)
                res <- withForeignPtr fpt \buf -> do
                    FFI.c_pin ctx vIsUnique (castPtr buf)

                peek vIsUnique >>= \case
                    CBool 0 -> pure ()
                    CBool _ -> modifyIORef' vPins (cpin:)

                pure res

        BAR bs@(BS.BS fpt wid) -> do
            modifyIORef' vTemp (bs:)
            withForeignPtr fpt \buf -> do
                FFI.c_bar ctx (fromIntegral wid) buf

        CONS x y -> do
            xv <- loop vPins vTemp x
            yv <- loop vPins vTemp y
            FFI.c_cons ctx xv yv


-- BLAKE3 Hashing --------------------------------------------------------------

{-# INLINE withContext #-}
withContext :: (FFI.Ctx -> IO a) -> IO a
withContext = bracket FFI.c_make FFI.c_free

{-# INLINE hash #-}
hash :: ByteString -> ByteString -> IO Hash256
hash hed bod =
    withContext \ctx -> hash' ctx hed bod

hash' :: FFI.Ctx -> ByteString -> ByteString -> IO Hash256
hash' ctx (BS.BS hedPtr hedWid) (BS.BS bodPtr bodWid) =
    let
        !hedSize = fromIntegral hedWid
        !bodSize = fromIntegral bodWid
    in
        allocaBytes 32 \outBuf ->
        withForeignPtr hedPtr \hedBuf ->
        withForeignPtr bodPtr \bodBuf ->
    do
        FFI.c_hash ctx outBuf hedSize hedBuf bodSize bodBuf
        peek $ castPtr outBuf
