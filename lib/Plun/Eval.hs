{-# LANGUAGE RecursiveDo #-}
{-# OPTIONS_GHC -Wall    #-}
{-# OPTIONS_GHC -Werror  #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Plun.Eval
    ( EvalSt(..)
    , state
    , Pln(..)
    , (%%)
    , toNat
    , cryptographicIdentity
    , executeLaw
    , compileLaw
    , valLawBody
    , mkPin
    , mkPin'
    , mkLaw
    )
where

import Data.Primitive.SmallArray
import Plun.Data
import Plun.Types
import PlunderPrelude            hiding (hash)

import Jar          (capBSExn, jar)
import Jar.Noun     (showJarBits)
import Jar.Nounable (Hash256(..), Nounable(..), NounView(..))

import qualified BLAKE3         as B3
import qualified Data.ByteArray as BA

-- Law Book State --------------------------------------------------------------

{-
    - `stBook` is a table of all rules, for deduplication.
    - `stFast` is a configurable optimizer.
-}
data EvalSt = EVAL_ST
    { stBook :: Map ByteString Pin
    , stFast :: Pin -> Pin
    }

makeFieldLabels ''EvalSt

state :: IORef EvalSt
state = unsafePerformIO $ newIORef $ EVAL_ST{..}
  where
    stBook = mempty
    stFast = \x -> x

loadPin :: MonadIO m => ByteString -> m (Maybe Pin)
loadPin hazh = liftIO $ do
    EVAL_ST{..} <- readIORef state
    pure $ lookup hazh stBook

-- Constructing Pins and Laws --------------------------------------------------

overBook :: (Map ByteString Pin -> Map ByteString Pin) -> EvalSt -> EvalSt
overBook f s = s { stBook = f (stBook s) }

mkLaw :: LawName -> Nat -> Pln -> Pln
mkLaw nam arg (force -> bod) =
    if arg==0 then AT 0 else
    fromMaybe law $ fmap (nodVal . DAT)
                  $ matchData nam arg bod
  where
    law = nodVal $ LAW
                 $ L nam arg bod
                 $ executeLaw
                 $ compileLaw nam arg bod

{-
    -   Jam body prefixed by reference-list.
    -   The reference list is terminated by the 0 hash.
-}
cryptographicIdentity :: Vector Pin -> ByteString -> ByteString
cryptographicIdentity refs jamBody =
    digestBytes
        $ B3.hash
        $ toList (fmap pinHash refs) <> [replicate 32 0, jamBody]
  where
    digestBytes :: B3.Digest 32 -> ByteString
    digestBytes d = BA.copyAndFreeze d (const $ pure ())

mkPin :: Pln -> Pln
mkPin = nodVal . PIN . unsafePerformIO . mkPin'

mkPin' :: MonadIO m => Pln -> m Pin
mkPin' inp = do
    -- let putNam tx = putStrLn ("\n\n==== [[[" <> tx <> "]]] ====\n")
    -- putNam (valName inp)
    core        <- evaluate (force inp)
    EVAL_ST{..} <- readIORef state
    (ref, byt)  <- jar core

    -- print ("JAR_BYTES" :: Text, length byt)
    -- showJarBits core

    -- Just for debugging purposes, we deserialize each pin and validate.
    capBSExn (nodVal . PIN <$> ref) byt >>= \case
        trip | trip==core -> pure ()
        trip              -> do putStrLn ("INP:" <> tshow core)
                                showJarBits core
                                putStrLn ("OUT:" <> tshow trip)
                                showJarBits trip
                                error "sour jam"

    let haz = cryptographicIdentity ref byt
        exe = case core of
                PLN _ (LAW l) -> \xs -> lawExec l (core:xs)
                v             -> foldl' (%%) v
        res = stFast (P ref byt haz exe core)

    -- TODO loadPin breaks lazy evaluation of the hash.
    -- Some way to avoid this?
    --
    -- Specifically, if we eagerly do global deduplication, then we
    -- must eagerly serialize and eagerly hash.  This is probably
    -- not too much of a problem, but it also isn't great.
    --
    -- For jets, we only need to check the hash if the tag matches.
    --
    -- Really, we want global deduplication on disk, and *eventual*
    -- deduplication in memory.
    --
    -- One possible solution is to have the hash be an
    -- `unsafePerformIO` thunk that first calculates the hash,
    -- and then deduplicates through mutation.
    --
    -- This would require making `pinItem` an IORef.  The performance
    -- impact of that would be relatively small.
    --
    -- However, pinExec would still contain a reference to `lawExec`,
    -- which would contain it's own copy of the compiled code and
    -- that code would contain references to all constant values
    -- referenced in the code.  That's potentially of significant size.
    --
    -- Making `pinExec` be an IORef probably *would* have a
    -- significant effect on performance.  That's an extra indirection
    -- and a dereference for every single function call.
    --
    -- Might be best to just accept the eager deduplication in the
    -- Haskell impl.  Will also make snapshotting faster.  Just write
    -- to disk what has already been computed.
    loadPin haz >>= \case
       Just loaded -> pure loaded
       Nothing     -> atomicModifyIORef' state $ \st ->
                        (,) (overBook (insertMap haz res) st)
                            res


-- Evaluation ------------------------------------------------------------------

infixl 5 %%;

(%%) :: Pln -> Pln -> Pln
(%%) (PLN 1 f) x = eval f [x]
(%%) (PLN n f) x = PLN (n-1) (APP f x)

eval :: Nod -> [Pln] -> Pln
eval nod args = case nod of
    APP pf px -> eval pf (px:args)
    NAT n     -> execNat n args
    LAW l     -> lawExec l (nodVal nod : args)
    PIN g     -> pinExec g args
    DAT dj    -> evalData args dj

execNat :: Nat -> [Pln] -> Pln
execNat 0 [n,a,b]     = mkLaw (LN $ toNat n) (toNat a) b
execNat 1 [p,l,a,n,x] = wut p l a n x
execNat 2 [z,p,x]     = case toNat x of { 0 -> z; n -> p %% AT(n-1) }
execNat 3 [x]         = AT (toNat x + 1)
execNat 4 [v]         = mkPin v
execNat _ _           = AT 0

toNat :: Pln -> Nat
toNat (PLN _ (NAT n)) = n
toNat (PLN _ _      ) = 0

wut :: Pln -> Pln -> Pln -> Pln -> Pln -> Pln
wut p l a n (PLN args nod) = case nod of
    PIN P{..} -> p %% pinItem
    LAW L{..} -> l %% AT (lawNameNat lawName) %% AT lawArgs %% lawBody
    APP f x   -> a %% (PLN (args+1) f) %% x
    NAT{}     -> n
    DAT dj    -> dataWut goLaw goApp dj
      where
        goApp x y      = a %% x %% y
        goLaw nm ar bd = l %% AT (coerce nm) %% AT ar %% bd

valLawBody :: Pln -> Pln
valLawBody = \case
    PLN _ (LAW(L{..})) -> lawBody
    PLN _ (PIN(P{..})) -> valLawBody pinItem
    PLN _ (DAT dj)     -> dataWut (\_ _ b -> b) (\_ _ -> AT 0) dj
    PLN _ _            -> AT 0

data Prog = PROG
    { arity :: !Int
    , stkSz :: !Int
    , prgrm :: !Run
    }
  deriving (Eq, Show)

data Run
    = LOG !Text !Run
    | CNS !Pln
    | REF !Int
    | KAL !Run !Run
    | LET !Int !Run !Run
  deriving (Eq, Show)

-- We don't have to care about overflow when converting to `Int`.
-- If the arity is that big, this will never be run.
compileLaw :: LawName -> Nat -> Pln -> Prog
compileLaw =
    \_n a b ->
        let (maxArg, code) = go a b
        in PROG (fromIntegral a)
                (fromIntegral maxArg + 1)
                code
  where
    go :: Nat -> Pln -> (Nat, Run)
    go maxArg = \case
        AT n | n<=maxArg -> (,) maxArg (REF $ fromIntegral n)
        NAT 0 :& f :# x  -> (,) (max fMax xMax) (KAL fRun xRun)
                              where (fMax, fRun) = go maxArg f
                                    (xMax, xRun) = go maxArg x
        NAT 1 :& v :# b  -> (,) (max vMax bMax) (LET idx vRun bRun)
                              where (vMax, vRun) = go (maxArg+1) v
                                    (bMax, bRun) = go (maxArg+1) b
                                    idx          = fromIntegral (maxArg+1)
        NAT 2 :# x       -> (maxArg, CNS x)
        x                -> (maxArg, CNS x)

executeLaw :: Prog -> [Pln] -> Pln
executeLaw PROG{stkSz,prgrm} args =
    unsafePerformIO $ do
        stk <- newSmallArray stkSz (error "impossible")

        let fill _  []     = pure ()
            fill !i (x:xs) = do writeSmallArray stk i x
                                fill (i+1) xs

        let go = \case
             LOG t x   -> putStrLn ("[" <> t <> "]") >> go x
             CNS v     -> pure v
             REF i     -> readSmallArray stk i
             KAL f x   -> (%%) <$> go f <*> go x
             LET i v b -> mdo vRes <- do { writeSmallArray stk i vRes ; go v }
                              go b

        fill 0 args
        go prgrm

-- Jar for Vals ----------------------------------------------------------------

instance Nounable Pln where
    type NounRef Pln = Pin
    mkCell = (%%)
    mkAtom = nodVal . NAT
    mkRefr = nodVal . PIN

    nounView = fromNod . valNod
      where
        fromLaw = \(LN n) a b -> APPISH (AT 0 %% AT n %% AT a) b
        fromNod = \case
            NAT n   -> NATISH n
            DAT d   -> dataWut fromLaw APPISH d
            APP f x -> APPISH (nodVal f) x
            LAW r   -> fromLaw (lawName r) (lawArgs r) (lawBody r)
            PIN p   -> REFISH p (Hash256 $ pinHash p)
