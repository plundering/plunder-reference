{-# OPTIONS_GHC -Wall   #-}
{-# OPTIONS_GHC -Werror #-}

module Sire.ReplExe
    ( main
    , showPlun
    , inlineFun
    , runBlockPlun
    , Refr
    , Global
    )
where

import PlunderPrelude
import Plun.Print
import Rainbow
import Rex
import Sire.Types
import Sire.Syntax
import Loot.Backend
import System.Directory

import Control.Monad.Except    (ExceptT(..), runExceptT)
import Control.Monad.State     (evalStateT)
import Control.Monad.State     (get)
import Control.Monad.State     (StateT(..))
import Data.ByteString.Builder (byteStringHex, toLazyByteString)
import Data.Text.IO            (hPutStr, hPutStrLn)
import Loot.ReplExe            (printValue, showPlun)
import Loot.Sugar              (resugarVal)
import Loot.Syntax             (valRex, joinRex, symbRex)
import Loot.Types              (Rul(..), Val(..), Bod(..), LawName(LN))
import Optics.Zoom             (zoom)
import Plun                    (pattern AT, (%%))
import Rex.Lexer               (isRuneChar)

import qualified Data.Map  as M
import qualified Plun      as P
import qualified Runner    as Run

--------------------------------------------------------------------------------

replActor :: MVar Pln -> MVar (Pln, Pln) -> Run.Ship -> IO a
replActor mRequest mReply initShip =
    evalStateT loop initShip
  where
    loop = do
        -- putStrLn "replActor: READMVAR"
        reqVal <- takeMVar mRequest
        assign #noun (AT 0 %% reqVal)
        Run.execFx
        Run.getResponse' >>= putMVar mReply
        loop

main :: IO ()
main = colorsOnlyInTerminal do
    filz <- fmap unpack <$> getArgs

    plunActor <- do
        mReq <- newEmptyMVar
        mRsp <- newEmptyMVar

        (inq, ship) <- Run.newTopShip "sire" Nothing (AT 0)

        tid <- async (replActor mReq mRsp ship)

        atomically $ writeTVar Run.vShips
                   $ M.singleton "sire" (inq, tid)

        pure \fx -> do
            -- shown <- showPlun fx
            -- putStrLn ("FX: " <> shown)
            putMVar mReq fx
            -- putStrLn "put fx, waiting for response"
            takeMVar mRsp

    vEnv <- newIORef mempty
    vMac <- newIORef mempty
    vLin <- newIORef mempty
    writeIORef P.vShowPlun (pure . showPlun)
    modifyIORef' P.state \st -> st { P.stFast = P.jetMatch }

    for_ filz (\p -> replFile p (runBlockPlun stdout False plunActor vEnv vMac vLin))
    welcome stdout
    replStdin (runBlockPlun stdout True plunActor vEnv vMac vLin)

welcome :: RexColor => Handle -> IO ()
welcome h = out txt
  where
    out =
        if (?rexColors == NoColors)
        then hPutStr h
        else hPutChunks h . singleton . bold . fore green . chunk

    txt = unlines
        [ ";"
        , "; ==== Sire REPL ===="
        , ";"
        , "; Since input is multi-line, there is currently no input-prompt."
        , "; Just type away!"
        , ";"
        , ""
        ]

runBlockPlun
    :: RexColor
    => Handle
    -> Bool
    -> (Pln -> IO (Pln, Pln))
    -> IORef (Map Symb Pln)
    -> IORef (Map Text Pln)
    -> IORef (Map Symb (Fun Pln Refr Global))
    -> Block
    -> IO ()
runBlockPlun h okErr actor vEnv vMac vLin block = do
    let go = runBlock h actor vEnv vMac vLin block
    let out = if (?rexColors == NoColors)
              then hPutStr stderr
              else hPutChunks stderr . singleton . bold . fore red . chunk
    runExceptT go >>= \case
        Right () -> pure ()
        Left err -> do
            out (dent ";;;" err)
            unless okErr $ do
                error "EXITING"

runBlock
    :: RexColor
    => Handle
    -> (Pln -> IO (Pln, Pln))
    -> IORef (Map Symb Pln)
    -> IORef (Map Text Pln)
    -> IORef (Map Symb (Fun Pln Refr Global))
    -> Block
    -> ExceptT Text IO ()
runBlock h actor vEnv vMacros vInline (BLK _ _ eRes) = do
    rexed  <- liftEither eRes
    vgs    <- newIORef (0::Nat)
    env    <- readIORef vEnv
    eVl    <- pure (valPlun $ envVal env)
    mac    <- readIORef vMacros
    parsed <- let ?macros = MacroEnv vgs eVl mac
              in liftEither (rexCmd rexed)
    env' <- runCmd h actor env vMacros vInline parsed
    writeIORef vEnv env'

cab :: Symb
cab = utf8Nat "_"

runCmd :: RexColor
       => Handle
       -> (Pln -> IO (Pln, Pln))
       -> Map Symb Pln
       -> IORef (Map Text Pln)
       -> IORef (Map Symb (Fun Pln Refr Global))
       -> XCmd
       -> ExceptT Text IO (Map Symb Pln)
runCmd h runFx scope vMacros vInline = \case
    PRINT v -> do
        G val _ <- resolveAndInjectExp scope vInline v
        liftIO $ printValue h True Nothing val
        pure $ insertMap cab val scope
    DUMPY v -> do
        G pln _ <- resolveAndInjectExp scope vInline v
        liftIO $ printValue h False Nothing pln
        pure scope
    CHECK checks -> do
        for checks $ \(raw, v) -> do
            G val _ <- resolveAndInjectExp scope vInline v
            let NAMED_CLOSURE _ _ top = nameClosure (loadShallow val)
            unless ((top::Val Symb) == 1)
                $ throwError . rexFile
                $ N OPEN "!=" [ T BARE_WORD "1" Nothing
                              , (joinRex . valRex . resugarVal mempty) top
                              ]
                $ Just . joinRex . (\rx -> N OPEN "??" rx Nothing)
                $ fmap (fmap absurd) raw
                    -- TODO Nope, definitly not impossible
        pure scope

    DEFINE [] ->
        pure scope

    DEFINE (BIND_EXP nam e : more) -> do
        G pln mInline <- resolveAndInjectExp scope vInline e
        case mInline of
            Nothing -> pure ()
            Just fn -> modifyIORef' vInline (insertMap nam fn)
        liftIO $ printValue h True (Just nam) pln
        case natUtf8 nam of
            Right txt | (not (null txt) && all isRuneChar txt) ->
                modifyIORef' vMacros (insertMap txt pln)
            _ -> pure ()
        runCmd h runFx (insertMap nam pln scope) vMacros vInline (DEFINE more)
    DEFINE (BIND_FUN nam f : more) -> do
        raw <- liftIO $ resolveTopFun f
        fun <- traverse (getRef scope vInline) raw
        modifyIORef vInline (insertMap nam fun)
        lam <- injectFun fun
        let pln = P.mkPin lam
        liftIO $ printValue h True (Just nam) pln
        modifyIORef' vInline (insertMap nam fun)
        case natUtf8 nam of
            Right txt | (not (null txt) && all isRuneChar txt) ->
                modifyIORef' vMacros (insertMap txt pln)
            _ -> pure ()
        runCmd h runFx (insertMap nam pln scope) vMacros vInline (DEFINE more)
    SAVEV v   -> do
        G pln _ <- resolveAndInjectExp scope vInline v
        hom <- liftIO getHomeDirectory
        has <- plunSave (hom <> "/.sire") pln
        ()  <- liftIO (outHex h has)
        let idn = utf8Nat (encodeBtc has)
        liftIO $ printValue h True (Just idn) pln
        pure $ insertMap idn pln scope
    IOEFF i r fx -> do
        G pln _ <- resolveAndInjectExp scope vInline fx
        liftIO $ printValue h True (Just $ utf8Nat "__FX__") pln
        (rid, res) <- liftIO (runFx pln)
        liftIO $ printValue h True (Just i) rid
        liftIO $ printValue h True (Just r) res
        pure $ insertMap i rid $ insertMap r res $ scope
    EPLOD _ -> do
        putStrLn "TODO: Macro debugger stubbed out for now"
        putStrLn "TODO: Need to reconsider how printing should work"
        putStrLn "TODO: Probably best to just print each expansion"
        putStrLn "TODO: Needed to stub because I deleted the expression"
        putStrLn "TODO: printer."
        pure scope

envVal :: ∀a. Map Symb a -> Val a
envVal =
    TAB . mapFromList . fmap f . mapToList
  where
    f :: (Symb, a) -> (Nat, Val a)
    f (nm, v) = (nm, REF v)

getRef :: Map Symb Pln
       -> IORef (Map Symb (Fun Pln Refr Global))
       -> Symb
       -> ExceptT Text IO Global
getRef env vInline nam = do
    pln <- maybe unresolved pure (lookup nam env)
    lin <- liftIO (readIORef vInline)
    pure (G pln (lookup nam lin))
  where
    unresolved = throwError ("Unresolved Reference: " <> showSymb nam)

resolveAndInjectExp
    :: Map Symb Pln
    -> IORef (Map Symb (Fun Pln Refr Global))
    -> XExp
    -> ExceptT Text IO Global
resolveAndInjectExp scope vInline ast = do
    self <- gensym (utf8Nat "self")
    argu <- gensym (utf8Nat "argu")
    body <- resolveExp mempty ast
    let rawFun = FUN self (LN 0) (argu:|[]) body
    func <- traverse (getRef scope vInline) rawFun
    expr <- traverse (getRef scope vInline) body
    pln <- injectFun func
    (_, _, mInline) <- expBod (1, mempty) expr
    pure $ G (valPlun (REF pln `APP` NAT 0)) mInline

injectFun :: Fun Pln Refr Global -> ExceptT Text IO Pln
injectFun (FUN self nam args exr) = do
    (_, b, _) <- expBod (nexVar, tab) exr
    let rul = RUL nam (fromIntegral ari) b
    pure (rulePlunOpt (gPlun <$> rul))
  where
    ari = length args
    nexVar = succ ari
    tab = mapFromList $ zip (refrKey <$> (self : toList args))
                            ((\v -> (0,v,Nothing)) . BVAR <$> [0..])

numRefs :: Int -> Exp a Refr b -> Int
numRefs k = \case
    EVAR r                 -> if refrKey r == k then 1 else 0
    EBED{}                 -> 0
    EREF{}                 -> 0
    ENAT{}                 -> 0
    EAPP f x               -> go f + go x
    ELIN xs                -> sum (go <$> xs)
    EREC _ v b             -> go v + go b
    ELET _ v b             -> go v + go b
    ELAM (FUN _ _ _ b)     -> min 1 (go b)
    --- Multiple references from a sub-functions only counts as one because
    --- it will be lambda-lifted (hence only used once).

    -- TODO Kill these features:
    EBAR{}                 -> 0
    ECOW{}                 -> 0
    ECAB{}                 -> 0
    ETAB ps                -> sum (go <$> ps)
    EVEC vs                -> sum (go <$> vs)
  where
    go = numRefs k

optimizeLet
    :: (Int, IntMap (Int, Bod Global, Maybe (Fun Pln Refr Global)))
    -> Refr
    -> Exp Pln Refr Global
    -> Exp Pln Refr Global
    -> ExceptT Text IO (Int, Bod Global, Inliner)
optimizeLet s@(nex, tab) refNam expr body = do
  let recurRef = numRefs k expr > 0
      multiRef = numRefs k body >= 2
  if
    trivialExp expr || (not recurRef && not multiRef)
  then do
    (varg, vv, mBodLin) <- expBod s expr
    let s' = (nex, insertMap k (varg, vv, mBodLin) tab)
    (barg, bb, mArgLin) <- expBod s' body
    let rarg = if (varg == 0) then 0 else barg
    pure (rarg, bb, mArgLin)
  else
    if not recurRef
    then do
      let s' = (nex+1, insertMap k (0, var nex, Nothing) tab)
      (varg, vv, mBodLin) <- expBod s' expr
      let s'' = (nex+1, insertMap k (0, var nex, mBodLin) tab)
      (barg, bb, _) <- expBod s'' body
      let rarg = if (varg == 0 || barg == 0) then 0 else barg-1
          -- TODO Why barg-1?  Shouldn't it just be `barg`?
      pure (rarg, BLET vv bb, Nothing)
    else do
      let s' = (nex+1, insertMap k (0, var nex, Nothing) tab)
      (varg, vv, _) <- expBod s' expr
      (barg, bb, _) <- expBod s' body
      let rarg = if (varg == 0 || barg == 0) then 0 else barg-1
          -- TODO Why barg-1?  Shouldn't it just be `barg`?
      pure (rarg, BLET vv bb, Nothing)
  where
    k = refrKey refNam
    var = BVAR . fromIntegral

-- Trivial if duplicating is cheaper than a let-reference.
trivialExp :: Exp v a b -> Bool
trivialExp = \case
    ENAT n -> n < 4294967296
    EBAR _ -> False
    EREF _ -> True
    EVAR _ -> True
    _      -> False

atomArity :: Nat -> Int
atomArity = fromIntegral . P.natArity

freeVars :: ∀a b. Fun a Refr b -> Set Refr
freeVars = goFun mempty
 where
    goFun :: Set Refr -> Fun a Refr b -> Set Refr
    goFun ours (FUN self _ args body) =
      let keyz = setFromList (self : toList args)
      in go (ours <> keyz) body

    go :: Set Refr -> Exp a Refr b -> Set Refr
    go ours = \case
        EBED{}     -> mempty
        ENAT{}     -> mempty
        EREF{}     -> mempty
        EVAR r     -> if (r `elem` ours)
                      then mempty
                      else singleton r
        ELAM f     -> goFun ours f
        EAPP f x   -> go ours f <> go ours x
        ELIN xs    -> concat (go ours <$> xs)
        EREC n v b -> let ours' = insertSet n ours
                      in go ours' v <> go ours' b
        ELET n v b -> let ours' = insertSet n ours
                      in go ours' v <> go ours' b

        -- TODO Kill these features
        EBAR{}     -> mempty
        ECOW{}     -> mempty
        ECAB{}     -> mempty
        ETAB ds    -> concat (go ours <$> toList ds)
        EVEC vs    -> concat (go ours <$> vs)

data Global = G { gPlun :: Pln, gInline :: Inliner }
  deriving (Generic)

instance Show Global where
  show (G (AT n) _) = "(AT " <> show n <> ")"
  show (G pln _) = "(G " <> show (P.valName pln) <> ")"

type Inliner = Maybe (Fun Pln Refr Global)


--  TODO Don't lift trivial aliases, just inline them (small atom, law)
--  TODO If we lifted anything, need to replace self-reference with a
--       new binding.
lambdaLift
    :: (Int, IntMap (Int, Bod Global, Inliner))
    -> Fun Pln Refr Global
    -> ExceptT Text IO (Exp Pln Refr Global)
lambdaLift _s f@(FUN self tag args body) = do
    let lifts = toList (freeVars f) :: [Refr]
    let liftV = EVAR <$> lifts
    let self' = self { refrKey = 2348734 }
    let body' = EREC self (app (EVAR self') liftV)  body
    let funct = FUN self' tag (derpConcat lifts args) body'
    pln <- injectFun funct
    pure $ app (EREF (G pln Nothing)) liftV
  where
    app fn []     = fn
    app fn (x:xs) = app (EAPP fn x) xs

    derpConcat :: [a] -> NonEmpty a -> NonEmpty a
    derpConcat xs ys = case xs <> toList ys of
                           []   -> error "Not possible"
                           z:zs -> z :| zs

-- TODO This only looks at `EVAR`, would be much shorter to write using
-- `uniplate` or whatever.
inlineTrivial
    :: (b, IntMap (Int, Bod Global, Inliner))
    -> Fun Pln Refr Global
    -> Fun Pln Refr Global
inlineTrivial s@(_, tab) (FUN self tag args body) =
    FUN self tag args (go body)
  where
    goFun = inlineTrivial s
    go = \case
        EBED b     -> EBED b
        ENAT n     -> ENAT n
        EREF r     -> EREF r
        EVAR v     -> case lookup (refrKey v) tab of
                        Nothing                                  -> EVAR v
                        Just (0, _, _)                           -> EVAR v
                        Just (_, BVAR{}, _)                      -> EVAR v
                        Just (_, BCNS (REF x), _)                -> EREF x
                        Just (_, BCNS (NAT n), _) | n<4294967296 -> ENAT n
                        _                                        -> EVAR v
        ELAM f     -> ELAM (goFun f)
        EAPP f x   -> EAPP (go f) (go x)
        ELIN xs    -> ELIN (go <$> xs)
        EREC n v b -> EREC n (go v) (go b)
        ELET n v b -> ELET n (go v) (go b)

        -- TODO Kill these features
        EBAR b     -> EBAR b
        ECOW n     -> ECOW n
        ECAB k     -> ECAB k
        ETAB ds    -> ETAB (go <$> ds)
        EVEC vs    -> EVEC (go <$> vs)

expBod
    :: (Int, IntMap (Int, Bod Global, Inliner))
    -> Exp Pln Refr Global
    -> ExceptT Text IO (Int, Bod Global, Inliner)
expBod s@(_, tab) = \case
    EBED b         -> do let ari = P.trueArity b
                         pure ( fromIntegral ari
                              , BCNS (REF (G b Nothing))
                              , Nothing
                              )
    ENAT n         -> pure (atomArity n, BCNS (NAT n), Nothing)
    ELAM f         -> do lifted <- lambdaLift s (inlineTrivial s f)
                         bodied <- expBod s lifted
                         let (arity, bod, _) = bodied
                         pure (arity, bod, Just f)
                           -- TODO Inlining Flag
    EVAR REFR{..}  -> pure $ fromMaybe (error "Internal Error")
                           $ lookup refrKey tab
    EREF (G t i)   -> pure ( fromIntegral (P.trueArity t)
                           , BCNS (REF (G t i))
                           , i
                           )

    EAPP f x -> do
        fR <- expBod s f
        xR <- expBod s x
        pure $ case (fR, xR) of
            ((_, fv, _),     (0,xv,_)     ) -> (0,   BAPP fv xv,       Nothing)
            ((0, fv, _),     (_,xv,_)     ) -> (0,   BAPP fv xv,       Nothing)
            ((1, fv, _),     (_,xv,_)     ) -> (0,   BAPP fv xv,       Nothing)
            ((a, BCNS fk, _),(_,BCNS xk,_)) -> (a-1, BCNS (APP fk xk), Nothing)
            ((a, fv, _),     (_,xv,_)     ) -> (a-1, BAPP fv xv,       Nothing)

    ELIN (f :| []) -> expBod s f

    ELIN (f :| (x:xs)) ->
        (lift $ runExceptT $ inlineExp tab f (x:|xs)) >>= \case
            Left _  -> expBod s (apple f (x:xs))
            Right e -> expBod s e
                -- traceM "INLINE SUCCESS"
                -- traceM "GOING DEEPER"
                -- traceM (show e)

    EREC n v b -> optimizeLet s n v b
    ELET n v b -> optimizeLet s n v b

    -- TODO Kill these features:
    EBAR n     -> pure (1, BCNS (BAR n), Nothing)
    ETAB ds    -> doTab ds
    EVEC vs    -> doVec vs
    ECOW n     -> pure (1+fromIntegral n, BCNS (COW n), Nothing)
    ECAB n     -> pure (1+(length n), BCNS (CAB n), Nothing)
  where
    apple f []    = f
    apple f (b:c) = apple (EAPP f b) c

    doVec vs = do
        es <- traverse (expBod s) vs

        let mkCow 0 = ROW mempty
            mkCow n = COW n

        let cow = (mkCow $ fromIntegral $ length vs)
        let ex = vecApp cow (view _2 <$> es)

        pure $ if all (> 0) (view _1 <$> es)
               then (1, ex, Nothing)
               else (0, ex, Nothing)

    doTab
        :: Map Nat (Exp Pln Refr Global)
        -> ExceptT Text IO (Int, Bod Global, Maybe (Fun Pln Refr Global))
    doTab ds = do
        let tups = M.toAscList ds
            keyz = fst <$> tups
            exps = snd <$> tups

        rs <- traverse (expBod s) exps

        let aris = view _1 <$> rs
            vals = view _2 <$> rs
            ex = vecApp (CAB $ setFromList keyz) vals
            ar = if all (> 0) aris then 1 else 0

        pure (ar, ex, Nothing)

    vecApp :: Val a -> [Bod a] -> Bod a
    vecApp cnstr params = cnsApp cnstr params

    bodApp f = \case []   -> f
                     x:xs -> bodApp (BAPP f x) xs

    cnsApp :: Val a -> [Bod a] -> Bod a
    cnsApp f = \case []          -> BCNS f
                     BCNS x : xs -> cnsApp (APP f x) xs
                     x      : xs -> bodApp (BCNS f)   (x:xs)

-- Name Resolution -------------------------------------------------------------

vGenSym :: IORef Int
vGenSym = unsafePerformIO (newIORef 0)

data Refr = REFR
    { refrName :: !Symb
    , refrKey  :: !Int
    }

instance Eq  Refr where (==)    x y = (==)    (refrKey x) (refrKey y)
instance Ord Refr where compare x y = compare (refrKey x) (refrKey y)

showSymb :: Symb -> Text
showSymb =
    let ?rexColors = NoColors
    in rexLine . symbRex

instance Show Refr where
    show (REFR n k) = unpack (showSymb n) <> "_" <> show k
    -- TODO Doesn't handle all cases correctly

gensym :: MonadIO m => Symb -> m Refr
gensym nam = do
    key <- readIORef vGenSym
    nex <- evaluate (key+1)
    writeIORef vGenSym nex
    pure (REFR nam key)

resolveFun :: Map Symb Refr -> Fun v Symb Symb -> IO (Fun v Refr Symb)
resolveFun env (FUN self tag args body) = do
    selfR <- gensym self
    argsR <- traverse gensym args
    envir <- pure (M.union
                   (mapFromList
                    (zip (self : toList args)
                         (selfR : toList argsR)))
                      env)
    bodyR <- resolveExp envir body
    pure (FUN selfR tag argsR bodyR)

resolveTopFun :: Fun v Symb Symb -> IO (Fun v Refr Symb)
resolveTopFun = resolveFun mempty

refreshRef
    :: Refr
    -> StateT (Int, Map Int Refr) IO Refr
refreshRef ref =
  (lookup (refrKey ref) . snd <$> get) >>= \case
      Just r  -> pure r
      Nothing -> pure ref

refreshBinder
    :: Refr
    -> StateT (Int, Map Int Refr) IO Refr
refreshBinder ref = do
  tab <- snd <$> get
  let key = refrKey ref
  case lookup key tab of
      Just r ->
          pure r
      Nothing -> do
          r <- zoom _1 (gensym $ refrName ref)
          modifying _2 (insertMap key r)
          pure r

duplicateFun
    :: Fun a Refr b
    -> StateT (Int, Map Int Refr) IO (Fun a Refr b)
duplicateFun (FUN self name args body) = do
    self' <- refreshBinder self
    args' <- traverse refreshBinder args
    body' <- duplicateExp body
    pure (FUN self' name args' body')

duplicateExpTop :: Exp a Refr b -> IO (Exp a Refr b)
duplicateExpTop e = evalStateT (duplicateExp e) (0, mempty)

-- Duplicates an expression, creating fresh Refrs for each binding.
duplicateExp
    :: ∀a b
     . Exp a Refr b
    -> StateT (Int, Map Int Refr) IO (Exp a Refr b)
duplicateExp = go
  where
    go
        :: Exp a Refr b
        -> StateT (Int, Map Int Refr) IO (Exp a Refr b)
    go expr = case expr of
        EVAR x      -> EVAR <$> refreshRef x
        EREC v e b  -> do v' <- refreshBinder v
                          EREC v' <$> go e <*> go b
        ELET v e b  -> do v' <- refreshBinder v
                          EREC v' <$> go e <*> go b
        EAPP f x    -> EAPP <$> go f <*> go x
        ELAM f      -> ELAM <$> duplicateFun f
        ELIN xs     -> ELIN <$> traverse go xs
        EBED{}      -> pure expr
        EREF{}      -> pure expr
        ENAT{}      -> pure expr

        -- TODO Remove these features
        EBAR{}      -> pure expr
        ECOW{}      -> pure expr
        ECAB{}      -> pure expr
        EVEC xs     -> EVEC <$> traverse go xs
        ETAB xs     -> ETAB <$> traverse go xs

inlineFun
    :: (Show a, Show b)
    => Fun a Refr b
    -> NonEmpty (Exp a Refr b)
    -> ExceptT Text IO (Exp a Refr b)
inlineFun (FUN self _ args body) params = do
    -- ceM ("INLINE:" <> (unpack $ natUtf8Exn name))
    -- ceM ("  BODY:" <> (intercalate "\n       " $ lines $ ppShow fn))
    -- ceM (show (length args, length params))
    -- _ (zip args params) \(a,p) -> do
        -- ceM ("   ARG:" <> show a)
        -- ceM ("      >" <> show p)
    case ( compare (length params) (length args)
         , numRefs (refrKey self) body
         )
      of
        (EQ, 0) -> do res <- liftIO $ duplicateExpTop
                                    $ foldr (uncurry ELET) body
                                    $ zip args params
                      -- ceM ("   OUT:" <> (intercalate "\n       " $ lines $ ppShow res))
                      pure res
        (EQ, _) -> noInline "Cannot inline recursive functions"
        (GT, _) -> noInline "Inline Expression has too many arguments"
        (LT, _) -> noInline "Inline Expression has too few arguments"

noInline :: MonadError Text m => Text -> m a
noInline msg = do
   -- ceM ("NOINLINE:" <> unpack msg)
   throwError msg

inlineExp
    :: IntMap (Int, Bod Global, Inliner)
    -> Exp Pln Refr Global
    -> NonEmpty (Exp Pln Refr Global)
    -> ExceptT Text IO (Exp Pln Refr Global)
inlineExp tab f xs =
    case f of
        ELAM l ->
            inlineFun l xs
        EREF (G _ Nothing) ->
            noInline "Not inlinable"
        EREF (G _ (Just fn)) ->
            inlineFun fn xs
        EVAR v ->
            case
                do (_,_,mL) <- lookup (refrKey v) tab
                   mL
            of
                Nothing -> noInline "Not a function"
                Just fn -> inlineFun fn xs
        _  ->
            noInline "Head of ! expression is not a known function"

resolveExp
    :: MonadIO m
    => Map Symb Refr
    -> Exp a Symb Symb
    -> m (Exp a Refr Symb)
resolveExp e = liftIO . \case
    EBED b     -> pure (EBED b)
    ENAT n     -> pure (ENAT n)
    EREF r     -> case lookup r e of
                    Nothing -> pure (EREF r)
                    Just rf -> pure (EVAR rf)

    -- TODO Should this be `(absurd v)`?
    EVAR v     -> case lookup v e of
                    Nothing -> pure (EREF v)
                    Just rf -> pure (EVAR rf)

    EAPP f x   -> EAPP <$> go e f <*> go e x
    ELIN xs    -> ELIN <$> traverse (go e) xs
    EVEC vs    -> EVEC <$> traverse (go e) vs
    EREC n v b -> do r <- gensym n
                     let e2 = insertMap n r e
                     EREC r <$> go e2 v <*> go e2 b
    ELET n v b -> do r <- gensym n
                     let e2 = insertMap n r e
                     ELET r <$> go e v <*> go e2 b

    ELAM f     -> ELAM <$> resolveFun e f

    -- TODO Remove the following features:
    ETAB ps    -> ETAB <$> traverse (go e) ps
    EBAR n     -> pure (EBAR n)
    ECOW n     -> pure (ECOW n)
    ECAB n     -> pure (ECAB n)
  where
    go = resolveExp


-- Hacky Snapshot System -------------------------------------------------------

bsHex :: ByteString -> Text
bsHex = decodeUtf8 . ("%0x" <>) . toStrict . toLazyByteString . byteStringHex

outHex :: RexColor => Handle -> ByteString -> IO ()
outHex h = out . bsHex
  where
    out = if (?rexColors == NoColors)
          then hPutStrLn h
          else hPutChunksLn h . singleton . bold . fore grey . chunk
