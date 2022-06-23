{-# OPTIONS_GHC -Wall   #-}
{-# OPTIONS_GHC -Werror #-}

module Sire.Sugar
    ( desugarCmd
    , resugarRul
    , resugarVal
    , varNames
    )
where

import PlunderPrelude       hiding (toList)
import Sire.Syntax
import Sire.Types

import Data.List ((!!))

import qualified Data.Foldable as Foldable


-- Desugaring ------------------------------------------------------------------

desugarCmd :: XCmd z -> Cmd z Text Text
desugarCmd (XVOPEN vs x)           = VOPEN vs (desugarExp x)
desugarCmd (XALIAS nvs)            = ALIAS (nvs <&> \(n,v) -> (n,desugarVal v))
desugarCmd (XPRINT v)              = PRINT (desugarExp v)
desugarCmd (XDUMPY v)              = DUMPY (desugarExp v)
desugarCmd (XCHECK cs)             = CHECK (cs <&> \c -> (c, desugarExp c))
desugarCmd (XDEFUN fs)             = DEFUN (desugarFun <$> fs)
desugarCmd (XSAVEV v)              = SAVEV (desugarExp v)
desugarCmd (XIOEFF i r e)          = IOEFF i r (desugarExp e)
desugarCmd (XMACRO v e)            = MACRO v (desugarExp e)
desugarCmd (XPLODE e)              = EPLOD e
desugarCmd (XMKRUL r@(XLAW t _ _)) = let tg = desugarTag t
                                     in MKRUL (tagIdn tg) (desugarRul r)

desugarTag :: XTag -> Tag
desugarTag = \case
    XTAG idn _ (Just nam) -> TAG idn nam
    XTAG idn _ Nothing    -> TAG idn (LN $ utf8Nat idn)

rawResugarTag :: Tag -> XTag
rawResugarTag = \case
    TAG "_" (LN 0) ->
        XTAG "_" Nothing Nothing
    TAG nm tg ->
        XTAG nm Nothing $
            if (LN $ utf8Nat nm) == tg
            then Nothing
            else Just tg

desugarVal :: XVal -> Val Text
desugarVal = \case
    XVREF g   -> REF g
    XVNAT n   -> NAT n
    XVAPP f x -> APP (desugarVal f) (desugarVal x)
    XVROW r   -> ROW (desugarVal <$> r)
    XVTAB t   -> TAB (desugarVal <$> t)
    XVBAR b   -> BAR b
    XVLAW l   -> desugarLaw l
    XVCOW n   -> COW n
    XVCAB n   -> CAB n

desugarFun :: XFun v -> (Text, Fun v Text Text)
desugarFun (XFUN t rs e) =
    (xtagIdn t, FUN (xtagIdn t) (xtagNam t) rs (desugarExp e))

desugarExp :: XExp v -> Exp v Text Text
desugarExp = \case
  XEBED b               -> EBED b
  XEREF t               -> EREF t
  XEHAZ h               -> EHAZ h
  XENAT n               -> ENAT n
  XEBAR n               -> EBAR n
  XEAPP f x             -> EAPP (go f) (go x)
  XEREC n v b           -> EREC n (go v) (go b)
  XELET n v b           -> ELET n (go v) (go b)
  XEVEC ps              -> EVEC (go <$> ps)
  XECOW n               -> ECOW n
  XECAB n               -> ECAB n
  XETAB rs              -> ETAB (go <$> rs)
  XELAM (XFUN n r b)    -> ELAM (goLam n r b)
  XELIN xs              -> ELIN (go <$> xs)
 where
  goLam t rs b = FUN (xtagIdn t) (lambTag t) rs (go b)
  go = desugarExp

  lambTag (XTAG _  _ (Just t)) = t
  lambTag (XTAG nm _ Nothing)  = LN (utf8Nat nm)

xtagIdn :: XTag -> Text
xtagIdn (XTAG n _ _) = n

xtagNam :: XTag -> LawName
xtagNam (XTAG n _ Nothing)  = LN (utf8Nat n)
xtagNam (XTAG _ _ (Just t)) = t

desugarRul :: XLaw -> Rul Text
desugarRul (XLAW t as b) =
    let ar = fromIntegral (length as)
        tg = desugarTag t
        nex = succ $ fromIntegral $ length as
        loc = mapFromList $ zip (tagIdn tg : as) [0..]
    in RUL (tagNam tg) ar (desugarBod nex loc b)

desugarLaw :: XLaw -> Val Text
desugarLaw l = LAW n a b
  where
    RUL n a b = desugarRul l

desugarBod :: Int -> Map Text Int -> XBod -> Bod Text
desugarBod = go
  where
    go n e = \case
        XVAR v     -> maybe (BCNS $ REF v) (BVAR . fromIntegral) (lookup v e)
        XCNS v     -> BCNS (desugarVal v)
        XAPP f x   -> BAPP (go n e f) (go n e x)
        XBAD v     -> BBAD (desugarVal v)
        XLET v x b -> BLET (go n' e' x) (go n' e' b)
                       where n' = n+1
                             e' = insertMap v n e


-- Resugaring ------------------------------------------------------------------

resugarVal :: Val Text -> XVal
resugarVal = \case
  REF x     -> XVREF x
  NAT n     -> XVNAT n
  APP f x   -> XVAPP (resugarVal f) (resugarVal x)
  ROW r     -> XVROW (resugarVal <$> r)
  LAW n a b -> XVLAW (resugarLaw n a b)
  TAB t     -> XVTAB (resugarVal <$> t)
  BAR b     -> XVBAR b
  COW n     -> XVCOW n
  CAB n     -> XVCAB n

-- TODO Need a symbol table in order to choose valid names.
resugarLaw :: LawName -> Nat -> Bod Text -> XLaw
resugarLaw n a b =
    let fb = if recursiveBod b then "f" else ""
    in case natUtf8 (lawNameNat n) of
           Left _   -> resugarRul fb (RUL n a b)
           Right "" -> resugarRul fb (RUL n a b)
           Right nm -> resugarRul nm (RUL n a b)

recursiveBod :: Bod a -> Bool
recursiveBod = \case
    BVAR 0 -> True
    BVAR _ -> False
    BCNS{} -> False
    BBAD{} -> False
    BAPP f x -> recursiveBod f || recursiveBod x
    BLET v x -> recursiveBod v || recursiveBod x

resugarRul :: Text -> Rul Text -> XLaw
resugarRul idn (RUL nam arg bod) =
    case
        XLAW yNam argNames <$> goBod isFree supply nex bod
    of
        Left err -> error ("IMPOSSIBLE: " <> unpack err)
        Right vl -> vl
  where
    isFree  = (`elem` freeSet)
    freeSet = setFromList (Foldable.toList bod) :: Set Text
    yNam = rawResugarTag $ TAG idn nam
    nex = 1 + arg
    argNames = take (fromIntegral arg) argSupply
    supply = idn : argSupply
    argSupply = filter (not . isUsed) varNames
    isUsed nm = isFree nm || nm==idn

isCode :: Nat -> Val a -> Bool
isCode nex (NAT n)                 = n<nex
isCode _   (NAT 0 `APP` _ `APP` _) = True
isCode _   (NAT 1 `APP` _ `APP` _) = True
isCode _   (NAT 2 `APP` _)         = True
isCode _   _                       = False

goBod :: (Text -> Bool) -> [Text] -> Nat -> Bod Text -> Either Text XBod
goBod isGlo names nex = \case
    BCNS v   -> if isCode nex v
                then pure $ XBAD (resugarVal v)
                else pure $ XCNS (resugarVal v)
                -- Anything code-shaped will automatically be
                -- constant-tagged (since it is required by correctness),
                -- this makes the explicit "!" unneccesary.
    BBAD v   -> pure $ XBAD (resugarVal v)
    BAPP f x -> XAPP <$> go nex f <*> go nex x
    BLET v k -> XLET (names!!(fromIntegral nex)) <$> go(nex+1) v <*> go(nex+1) k
    BVAR v   -> if v >= nex
                then pure $ XBAD (XVNAT 0 `XVAPP` XVNAT (fromIntegral v))
                else pure $ XVAR (names!!(fromIntegral v))
  where
    go = goBod isGlo names

varNames :: [Text]
varNames = fmap pack strs
 where
  alphabet = ['a'..'z']
  strs = fmap singleton alphabet <> do
           var    <- strs
           letter <- alphabet
           pure (var <> [letter])
