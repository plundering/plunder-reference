{-# OPTIONS_GHC -Wall   #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Werror #-}

{-|
    Bit-oriented serialization of noun-like data, with deduplication.

    This code will work with any data structure that can be converted
    to a noun (binary trees with natural numbers at the leaves) and back.

    TODO Verify that nothing comes after there final `1` bit.

    TODO Error out if we hit the end of the buffer during decoding.

        (Stop doing the thing where we treat the end as an infinite
        stream of zeros).

    TODO Manually inline everything that is only used once.

    TODO getBit can be smarter: Just `getBit` on loaded word at offset.

    TODO Hashable instance for ByteString uses length.  That's the worst
         possible implementation for cryptographic-hash keys, since they
         all have the same length and have uniformly distributed contents.

    TODO Consider using `vector-hashtables`.

    TODO Faster reading of tags by reading two bits instead of reading
        one bit (possibly twice)?

    # De-Serialization

    ## TODO: Do two scans accross the buffer.

    On the first scan, build up the set of all backreferenced positions.
    This way, we wont need to build up the massive table as we
    deserialize.  Only *actually used* positions will go in the table.

    Also build up a set of positions that we backreference from.
    This should be stored as an array.

    Then, when we deserialize, we have several arrays:

        targets: Ptr Word
        arrows: Ptr (Word, Int)
        nextTargetIdx: Int
        backs: MutableArray Val

    At each new position, we check the bit-offset against the next target
    and the next arrow (both cached in registers).

    -   If we hit a target, we write it into the `backs` table and we
        bump the `targets` pointer.

    -   If we hit an arrow, we index into the `backs` table and we bump
        the `targets` pointer.

    This way, we never need to build the backreferences hashtable,
    and hash-table lookups are replaced by word-equality checks on
    two registers.

    We also no longer need to mess around with all the allocations needed
    when building up the hash table.

    This will likely have a huge performance impact.  If it doesn't,
    then that means that the bit-munging is the bottleneck, which would
    be surprising.

    # TODO: In the first pass, verify that input is in normal form.

    Throw an exception otherwise (can have a flag that disables the
    check).

    (Actually, can probably just have mkCell return an Either) and throw
    that as an exception if so?

    # TODO: Merge `targets` and `arrows` arrays

    No position may be a target *and* an arrow, so we can just have the
    arrows table:

        arrows: Ptr (Word, Int)

    And have a negative index indicate that we should write into the
    table instead of reading from it.  (I suppose -1 should indicate
    index 0, -2 should indicate index 1, etc)

    This way we also do not need another register to track the next
    target index for writting.

    ## Decodeing Atom Size-Of-Size (Unary)

    - Peek at the next word.
    - Calculate the number of least-significant bits in that word (there's
      a primitive for this).
    - Advance by that number of bits.
    - Return the number of bits


    ## In order to peek at the next Word64:

    - If we are past the end of the buffer:
        - Return zero.
    - If the bit-offset is zero:
        - Just peek.
    - If we are pointing to the last word:
        - Peek and right-shift by the bit offset.
    - Otherwise,
        - Peek the current word *and* the next word.
        - Right-shift the current word by the bit-offset.
        - Left-shift the next word by the bit-offset.
        - Binary or the resulting two words.

    ## To read the payload of an atom:

    Get n bits, where n > 64:

    - Get (n/64) words.
    - Advance by n bits.
    - Calculate an offset (equal to the current bit-offset)
    - Calculate the length (equal to n)
    - Construct a bit-vector using the buffer*length*offset.

    ## To load <64 bits.

    - Peek at the next word.
    - Mask the n lowest bits from the word.
    - Advance by that number of bits.
    - Return the word.

    ## Peeking at the next word:

    -   TODO End-detection logic is higly suspect.
    -   TODO Reconsider approach to end-of-input detection.
    -   TODO Stop assuming that we can read a word from the end of a ByteString
        whose size isn't a multiple of 8.

    Maybe we should work with a `Bar` newtype that has this as a
    type-system guarentee?  That would make conversions with Nat faster
    too.
-}
module Jar
    ( Hash256(..)
    , Nounable(..)
    , NounView(..)
    , jar
    , cap
    , capExn
    , capBS
    , capBSExn
    , DecodeErr
    )
where

import Data.Void
import Jar.Nounable
import Natty
import PlunderPrelude hiding (hash, (%))

import Data.Bits                 (clearBit, setBit, shiftL, shiftR)
import Data.Bits                 ((.&.), (.|.))
import Data.Vector.Primitive     ((!))
import Foreign.Marshal.Alloc     (callocBytes, free, mallocBytes, reallocBytes)
import Foreign.Marshal.Alloc     (allocaBytes)
import Foreign.Ptr               (Ptr, castPtr, plusPtr, ptrToWordPtr)
import Foreign.Storable          (peek, poke, pokeElemOff)
import GHC.Int                   (Int(I#))
import GHC.Integer.GMP.Internals (BigNat)
import GHC.Natural               (Natural(NatJ#, NatS#))
import GHC.Prim                  (Word#, ctz#, plusWord#, word2Int#)
import GHC.Word                  (Word(..))
import Text.Printf               (printf)

import qualified Data.ByteString.Unsafe   as BS
import qualified Data.HashTable.IO        as HT
import qualified Data.Vector              as V
import qualified Data.Vector.Mutable      as VM
import qualified Data.Vector.Primitive    as VP


-- Debugging Utils -------------------------------------------------------------

{-# INLINE debugM #-}
debugM :: Monad m => String -> m ()
debugM _ = pure ()
-- debugM = traceM

{-# INLINE debugMId #-}
debugMId :: (Monad m, Show a) => String -> m a -> m a
debugMId _ a = a
-- debugMId s a = traceM s >> a


-- Types -----------------------------------------------------------------------

data ValTreeState a s = VTS
    { refs :: {-# UNPACK #-} !(VM.MVector s a)
    , next :: {-# UNPACK #-} !Int
    }

-- TODO Add back debug context (end_ptr alone is useless)
data DecodeErr
    = EndOfInput (Ptr Word)
    | BadEncoding (Ptr Word) String
  deriving (Show, Eq, Ord)

instance Exception DecodeErr


-- Exports ---------------------------------------------------------------------

jar
    :: (MonadIO m, Nounable a, Eq a, Show a, Hashable a)
    => a
    -> m (Vector (NounRef a), ByteString)
jar val =
    liftIO $
    allocaBytes (8*6) \pRegs -> do
        !pins <- findRefs val
        (!wid, !tab) <- compress pRegs pins val
        !buf <- serialize pRegs tab wid pins val
        free tab
        pure (pins, Natty.dropTrailingZeros buf)


findRefs :: ∀a. Nounable a => a -> IO (Vector (NounRef a))
findRefs topVal = do
    refsTab :: HT.CuckooHashTable Hash256 Int <- HT.newSized 256

    vSt <- do v <- VM.new 256
              newIORef (VTS v 0)

    let goVal :: a -> IO ()
        goVal vl =
            case nounView vl of
                NATISH _   -> pure ()
                APPISH x y -> goVal x >> goVal y
                REFISH v h -> HT.lookup refsTab h >>= \case
                    Just{}  -> pure ()
                    Nothing -> pushRefr v h

        pushRefr :: NounRef a -> Hash256 -> IO ()
        pushRefr pin haz = do
            VTS{..} <- readIORef vSt
            newRefs <- if (next < VM.length refs)
                       then pure refs
                       else VM.unsafeGrow refs (VM.length refs)
            VM.write newRefs next pin
            HT.insert refsTab haz next
            writeIORef vSt (VTS newRefs (next+1))

    ()      <- goVal topVal
    VTS{..} <- readIORef vSt
    V.freeze (VM.take next refs)



-- Calculate Jam Size and Backrefs ---------------------------------------------

{-# INLINE atomSz #-}
atomSz :: Natural -> Word
atomSz 0  = 3
atomSz !a = W# (2## `plusWord#` preW
                    `plusWord#` preW
                    `plusWord#` atmW)
  where
    atmW = Natty.natBitWidth# a
    preW = Natty.wordBitWidth# atmW

{-# INLINE refSz #-}
refSz :: Word -> Word
refSz = atomSz . fromIntegral

{-
    This pre-computes the buffer-size of the result as well as the
    backreferences table.

    The has a big impact on performance.  The pre-calculated buffer size
    and pre-calculated backreferences table means that the actual
    serialization process doesn't ever need to allocate, just dump words
    into a buffer.

    Since the backreferences table is consumed linearly (every key is
    used exactly once, and the keys are consumed in order), we store the
    table as a [(Word,Word)] encoded as an array of words (alternating
    keys and values).
-}
compress
    :: ∀v
     . (Eq v, Show v, Hashable v, Nounable v)
    => Ptr Void
    -> Vector (NounRef v)
    -> v
    -> IO (Word, Ptr Word)
compress pRegs !pins !top = do
    let !pBuf :: Ptr (Ptr Word) = pRegs `plusPtr` 0
        !pWid :: Ptr Int        = pRegs `plusPtr` 8
        !pNex :: Ptr Int        = pRegs `plusPtr` 16
        !pNth :: Ptr Word       = pRegs `plusPtr` 24

    !nodes :: HT.BasicHashTable v Word <- HT.newSized 128

    for_ (zip [0,1..] (toList pins)) \(i,p) ->
        HT.insert nodes (mkRefr p) i

    let initialWidth = 256

    bufPtr <- mallocBytes (8*initialWidth)

    poke pBuf bufPtr
    poke pWid initialWidth
    poke pNex 0
    poke pNth (fromIntegral $ length pins)

    let appendBak :: Word -> Word -> IO ()
        appendBak !from !toLoc = do
            !nex <- peek pNex
            !wid <- peek pWid
            !old <- peek pBuf
            !buf <- if (nex+1) < wid
                    then pure old
                    else do buf <- reallocBytes old (8*wid*2)
                            poke pBuf buf
                            poke pWid (wid*2)
                            pure buf
            pokeElemOff buf nex     from
            pokeElemOff buf (nex+1) toLoc
            poke pNex (nex+2)

        proc :: Bool -> v -> IO Word
        proc alreadySeen inp = do
            n <- peek pNth
            unless alreadySeen (HT.insert nodes inp n)
            debugM $ show $ ( "noun" :: Text
                            , inp
                            , "at position" :: Text
                            , n
                            , if alreadySeen then "seen" else "" ::Text
                            )
            poke pNth (n+1)

            case nounView inp of
                NATISH x   -> pure (atomSz x)
                REFISH _ _ -> error "Impossible, these are always backreferences"
                APPISH h t -> do
                    !hSz <- go h
                    !tSz <- go t
                    pure (1+hSz+tSz)

        backref :: v -> Word -> IO ()
        backref inp bak = do
            n <- peek pNth
            debugM $ show (n, bak, inp)
            appendBak n bak

        go :: v -> IO Word
        go !inp = do
            HT.lookup nodes inp >>= \case
                Nothing  -> proc False inp
                Just bak -> do
                    let br = fromIntegral bak
                    let rf = backref inp bak $> refSz bak

                    -- TODO This (no-backref if smaller than actual
                    -- reference) logic should be applied to all nouns,
                    -- not just atoms.
                    case nounView inp of
                        REFISH _ _        -> rf
                        APPISH _ _        -> rf
                        NATISH x   | br<x -> rf
                        NATISH _          -> proc True inp

    !siz <- go top
    ()   <- appendBak maxBound maxBound
    !buf <- peek pBuf

    pure (siz+1, buf)

-- Serialization ---------------------------------------------------------------

{- |
    Backreferences are encoded as a [(Word,Word)], in sort order,
    serialized into native C array, where the end of the array is
    `WORDMAX`.

    `s.nex` is always equal to `*(s.pos)`.

    We output a backref whenever the current position equals (nex s).
    We load the backref target from `s.pos[1]` and bump `s.pos += 2`
    to move onto the next reference.  We then set `s.nex=*(s.pos)`
    to maintain the invariant.

    # For (TODO:what), write the register to the output; increment the outptr.

    # To write a bit:

        reg  |= 1 << off
        off <- (off + 1) % 64
        if (!off):
            buf[w++] <- reg
            reg      <- 0

    # To write a 64bit word:

        reg |= w << off
        buf[bufI++] = reg
        reg = w >> (64 - off)

    # To write some bits (< 64) from a word:

        wor = takeBits(wid, wor)
        reg = reg .|. (wor << off)
        off = (off + wid) % 64
        if (off + wid >= 64)
            buf[w] = x
            reg    = wor >> (wid - off)
-}
serialize
    :: ∀v
     . Nounable v
    => Ptr Void
    -> Ptr Word
    -> Word
    -> Vector (NounRef v)
    -> v
    -> IO ByteString
serialize pRegs !tbl !sz !pins !tri = do
    let !pPtr :: Ptr (Ptr Word) = pRegs `plusPtr` 0
        !pReg :: Ptr Word       = pRegs `plusPtr` 8
        !pOff :: Ptr Int        = pRegs `plusPtr` 16
        !pNix :: Ptr Word       = pRegs `plusPtr` 24
        !pTab :: Ptr (Ptr Word) = pRegs `plusPtr` 32
        !pNex :: Ptr Word       = pRegs `plusPtr` 40

    let !numPins = fromIntegral (length pins)

    let divUp x y = (x `div` y) + (if x `mod` y == 0 then 0 else 1)

    let wordSz = fromIntegral (sz `divUp` (64 :: Word))
        byteSz = fromIntegral (sz `divUp` (8 :: Word))

    outputBuffer <- callocBytes (wordSz*8)
    firstBackRef <- peek tbl

    poke pPtr outputBuffer
    poke pReg 0
    poke pOff 0
    poke pNix numPins
    poke pTab tbl
    poke pNex firstBackRef

    let getJarRef :: IO (Maybe Word)
        getJarRef = do
            nix <- peek pNix
            nex <- peek pNex
            if (nix /= nex)
            then do
                poke pNix (nix+1)
                pure Nothing
            else do
                tab <- peek pTab
                let tab' = tab `plusPtr` 16
                nex'   <- peek tab'
                target <- peek (tab `plusPtr` 8)
                poke pTab tab'
                poke pNex nex'
                pure (Just target)

    let flush = do
            ptr <- peek pPtr
            reg <- peek pReg
            poke ptr reg
            poke pPtr (ptr `plusPtr` 8)
            pure ()

    let writeBit b = do
            reg <- peek pReg
            off <- peek pOff
            poke pReg ((if b then setBit else clearBit) reg off)
            poke pOff ((off + 1) `mod` 64)
            when (off == 63) do
                flush
                poke pReg 0
                poke pOff 0

    let writeBits :: Int -> Word -> IO ()
        writeBits wid wor = do
            wur <- pure (Natty.takeBitsWord wid wor)
            reg <- peek pReg
            off <- peek pOff
            let newReg = reg .|. shiftL wur off
            let newOff = (off+wid) `mod` 64
            poke pReg newReg
            poke pOff newOff
            when ((wid+off) >= 64) do
                flush
                poke pReg (shiftR wur (wid - newOff))

    -- Write all of the signficant bits of a direct atom.
    let writeLastWord# :: Word# -> IO ()
        writeLastWord# w = do
            let bits = I# (word2Int# (Natty.wordBitWidth# w))
            writeBits (bits-1) (W# w)

    let  writeWord :: Word -> IO ()
         writeWord wor = do
             reg <- peek pReg
             off <- peek pOff
             poke pReg (reg .|. shiftL wor off)
             flush
             poke pReg (shiftR wor (64-off))

    -- Write all of the the signficant bits of an indirect atom.
    -- TODO Use memcpy when the bit-offset of the output is divisible by 8.
    let writeNatBigNat :: BigNat -> IO ()
        writeNatBigNat !(Natty.bigNatWords -> wordz) = do
          let lastIdx = VP.length wordz - 1
          for_ [0..(lastIdx-1)] $ \i ->
              writeWord (wordz ! i)
          let !(W# w) = (wordz ! lastIdx)
          writeLastWord# w

    let writeMat :: Natural -> IO ()
        writeMat 0   = writeBit True
        writeMat atm = do
            let atmWid = Natty.natBitWidth atm
            let preWid = fromIntegral (Natty.wordBitWidth atmWid)
            writeBits (preWid+1) (shiftL 1 preWid)
            writeBits (preWid-1) atmWid
            case atm of
                NatS# wd -> writeLastWord# wd
                NatJ# bn -> writeNatBigNat bn

    let writeTree :: v -> IO ()
        writeTree !n =
            getJarRef >>= \case
                Just bk -> do
                    writeBit True
                    writeBit False
                    writeMat (fromIntegral bk)
                Nothing -> case nounView n of
                    REFISH{} -> error "impossible"
                    NATISH a -> do
                        writeBit True
                        writeBit True
                        writeMat a
                    APPISH h t -> do
                        writeBit False
                        writeTree h
                        writeTree t

    writeTree tri

    writeBit True -- This way, we never need to invent zeros at the end
                  -- while deserializing.  If we hit the end of the
                  -- buffer, it's an error.

    peek pOff >>= \case
        0 -> pure ()
        _ -> flush

    BS.unsafePackCStringFinalizer
        (castPtr outputBuffer)
        byteSz
        (free outputBuffer)


-- De-Serialization ------------------------------------------------------------

capBS :: (Show a, Nounable a) => Vector a -> ByteString -> Either DecodeErr a
capBS refs bs
    = unsafePerformIO
    $ try
    $ BS.unsafeUseAsCStringLen bs
    \ (ptr, len)
    → allocaBytes (8*3)
    \ pRegs
    → deserialize pRegs refs (castPtr ptr) len

deserialize
    :: ∀a
     . (Show a, Nounable a)
    => Ptr Void
    -> Vector a
    -> Ptr Word8
    -> Int
    -> IO a
deserialize pRegs externalRefs bufPtr bufLen = do
    let !pPos  :: Ptr Word       = pRegs `plusPtr` 0
        !pPtr  :: Ptr (Ptr Word) = pRegs `plusPtr` 8
        !pUsed :: Ptr Word       = pRegs `plusPtr` 16

    poke pPos  (fromIntegral $ length externalRefs)
    poke pPtr  (castPtr bufPtr)
    poke pUsed 0

    let endPtr = bufPtr `plusPtr` bufLen

    refsTab <- do
        tab <- HT.newSized 128
        V.iforM_ externalRefs \i v ->
            HT.insert tab (fromIntegral i) v
        pure (tab :: HT.BasicHashTable Word a)

    let advance 0 = debugM "    advance: 0" >> pure ()
        advance !n = do
            debugM ("    advance: " <> show n)
            curPtr <- peek pPtr
            oldUsed <- peek pUsed
            let newUsed = n + oldUsed
            let nextPtr = plusPtr curPtr
                        $ (8 * (fromIntegral $ newUsed `div` 64))
            poke pPtr nextPtr
            poke pUsed (newUsed `mod` 64)
            pure ()

    let peekNextWord = debugMId "peekNextWord" $ do
            cur <- peek pPtr
            when (cur > endPtr) $ throwIO $ EndOfInput endPtr -- TODO
            let pTarget = cur `plusPtr` 8
            if ptrToWordPtr pTarget >= ptrToWordPtr endPtr
                then pure 0
                else peek pTarget

    let peekCurWord = debugMId "  peekCurWord" $ do
            cur <- peek pPtr
            when (cur > endPtr) $ throwIO $ EndOfInput endPtr -- TODO
            peek cur

    let peekWord = debugMId "  peekWord" $ do
            off <- peek pUsed
            cur <- peekCurWord
            nex <- peekNextWord
            let res = swiz off (cur, nex)
            let lol = printf "%064b" (fromIntegral res :: Integer)
            let wut = printf "%064b" (fromIntegral nex :: Integer)
            debugM ("\t" <> reverse lol)
            debugM ("\t" <> reverse wut)
            pure res
          where
            swiz :: Word -> (Word, Word) -> Word
            swiz !(fromIntegral -> off) (!low, !hig) =
                (.|.) (shiftR low off) (shiftL hig (64-off))

    let getWord = debugMId "getWord" $ do
            res <- peekWord
            advance 64
            pure res

    let getWordBits !n = debugMId ("  getWordBits(" <> show n <> ")") $ do
            w <- peekWord
            advance n
            debugM ("  getWordBits: " <> show (takeLowBits n w))
            pure (takeLowBits n w)
          where
            takeLowBits :: Word -> Word -> Word
            takeLowBits 64   !wor = wor
            takeLowBits !wid !wor = (2^wid - 1) .&. wor

    let getNatSizeOfSize = debugMId "    getNatSizeOfSize" $ do
            word@(W# w) <- peekWord
            when (word==0) $ error "impossible size-of-size"
            let res = W# (ctz# w)
            advance (res+1)
            pure res

    let getNatSize = debugMId "  getNatSize" $ do
            getNatSizeOfSize >>= \case
                0 -> pure 0
                e -> do ((2^(e-1)) .|.) <$> getWordBits (e-1)

    let getRef = debugMId "getRef" $ do
            res <- getNatSize >>= \case
                0 -> pure 0
                1 -> pure 1
                n -> do res <- getWordBits (n-1)
                        pure (setBit res (fromIntegral n - 1))
            debugM ("REF:" <> show res)
            pure res

    let getNatBits !(fromIntegral -> bits) = do
            debugMId ("getNatBits(" <> show bits <> ")") $ do
              res <- fmap wordsNat $
                VP.generateM bufSize $ \i -> do
                  debugM (show i)
                  if (i == lastIdx && numExtraBits /= 0)
                  then getWordBits (fromIntegral numExtraBits)
                  else getWord
              pure (setBit res bits)
          where
            bufSize      = numFullWords + min 1 numExtraBits
            lastIdx      = bufSize - 1
            numFullWords = bits `div` 64
            numExtraBits = bits `mod` 64

    let getNat = debugMId "getNat" $ do
            res <- getNatSize >>= \case
                0 -> pure 0
                1 -> pure 1
                n -> getNatBits (n-1)
            debugM ("NAT:" <> show res)
            pure res

    let getBit = debugMId "getBit" $ do
            word <- peekCurWord
            used <- fromIntegral <$> peek pUsed
            advance 1
            pure (0 /= shiftR word used .&. 1)

    let getVal = do
            p <- peek pPos
            getBit >>= \case
                False -> do
                    poke pPos (p+1)
                    res <- mkCell <$> getVal <*> getVal
                    debugM ("@" <> show p <> "\t" <> show res)
                    HT.insert refsTab p res
                    pure res
                True -> getBit >>= \case
                    True -> do
                        poke pPos (p+1)
                        res <- mkAtom <$> getNat
                        HT.insert refsTab p res
                        debugM ("@" <> show p <> "\t" <> show res)
                        pure res
                    False -> do
                        -- TODO BadEncoding should include context information.
                        !ref <- getRef
                        HT.lookup refsTab ref >>= \case
                            Just no -> pure no
                            Nothing -> do
                                let msg = "Invalid Reference: " <> show ref
                                throwIO (BadEncoding endPtr msg)


    getVal

capBSExn :: (Show a, Nounable a, MonadIO m) => Vector a -> ByteString -> m a
capBSExn refs bs =
    either throwIO pure (capBS refs bs)

cap :: (Show a, Nounable a) => Vector a -> Nat -> Either DecodeErr a
cap refs = capBS refs . natBytes

capExn :: (Show a, Nounable a, MonadIO m) => Vector a -> Nat -> m a
capExn refs = capBSExn refs . natBytes
