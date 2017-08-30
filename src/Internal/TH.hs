{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Internal.TH where

import Control.Monad.Primitive
import Control.Monad.ST.Strict
import Control.Monad.Trans.Class
import Data.Complex
import Data.Monoid (Dual(..), Endo(..))
import Data.Semigroup (Semigroup(..))
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Ptr (Ptr)
import Foreign.Marshal.Array (advancePtr)
import Foreign.Marshal.Utils (with)
import GHC.TypeLits
import Language.Haskell.TH

import Internal.Int (I, N(..))
import Internal.Matrix
import Internal.Vector (V(..))
import Internal.Writer


newtype C a = C { getC :: a }

newtype Hs a = Hs { getHs :: a }

newtype HsExp = HsExp { getHsExp :: Exp }

class Build a where
  data Acc a
  type M a
  type D a
  type Nm a
  build :: Monoid (Acc a) => Nm a -> (M a -> WriterT (Acc a) Q Type) -> Q a
  bind :: String -> WriterT (Acc a) Q (Nm a)
  order :: WriterT (Acc a) Q ()
  uplo :: Name -> Name -> Nm a -> WriterT (Acc a) Q ()
  trans :: Name -> Name -> Nm a -> WriterT (Acc a) Q ()
  diag :: Name -> Name -> Nm a -> WriterT (Acc a) Q ()
  dim :: Name -> Name -> Nm a -> WriterT (Acc a) Q (D a)
  band :: Name -> Name -> Nm a -> WriterT (Acc a) Q ()
  vec :: Nm a -> M a -> D a -> Q Type -> WriterT (Acc a) Q ()
  scalar :: Nm a -> Q Type -> WriterT (Acc a) Q ()
  mat :: Name -> Name -> Name -> Name -> Nm a -> M a -> D a -> D a -> Q Type -> WriterT (Acc a) Q ()
  packMat :: Name -> Name -> Name -> Nm a -> M a -> D a -> D a -> Q Type -> WriterT (Acc a) Q ()

tellC :: (Q Type -> Q Type) -> WriterT (Acc (C Type)) Q ()
tellC = tell . AccC . Endo

instance Build (C Type) where
  newtype Acc (C Type) = AccC { getType :: Endo (Q Type) }
    deriving (Monoid, Semigroup)
  type M (C Type) = ()
  type D (C Type) = ()
  type Nm (C Type) = ()

  build _ go = do
    (a, finish) <- runWriterT (go ())
    let r = calling a [t| IO $(pure a) |] [t| Ptr $(pure a) -> IO () |]
    C <$> (appEndo . getType) finish r

  bind _ = pure ()

  order = tellC $ \r -> [t| I -> $r |]

  uplo _ _ _ = tellC $ \r -> [t| I -> $r |]

  trans _ _ _ = tellC $ \r -> [t| I -> $r |]

  diag _ _ _ = tellC $ \r -> [t| I -> $r |]

  dim _ _ _ = tellC $ \r -> [t| I -> $r |]

  band _ _ _ = tellC $ \r -> [t| I -> $r |]

  vec _ _ _ a = tellC $ \r -> [t| Ptr $a -> I -> $r |]

  scalar _ _a = lift _a >>= \_a -> calling _a byValue byRef
    where
      byValue = tellC $ \r -> [t| $_a -> $r |]
      byRef = tellC $ \r -> [t| Ptr $_a -> $r |]

  mat _ _ _ _ _ _ _ _ a = tellC $ \r -> [t| Ptr $a -> I -> $r |]

  packMat _ _ _ _ _ _ _ a = tellC $ \r -> [t| Ptr $a -> $r |]

tellHsType :: (Q Type -> Q Type) -> WriterT (Acc (Hs Type)) Q ()
tellHsType = tell . AccHsType . Endo

instance Build (Hs Type) where
  newtype Acc (Hs Type) = AccHsType { getHsType :: Endo (Q Type) }
    deriving (Monoid, Semigroup)
  type M (Hs Type) = Name
  type D (Hs Type) = Type
  type Nm (Hs Type) = ()

  build _ go = do
    s <- newName "s"
    (a, finish) <- runWriterT (go s)
    let
      tvars = [ KindedTV s StarT ]
      ctx = pure []
      r = [t| ST $(varT s) $(pure a) |]
    (<$>) Hs $ forallT tvars ctx $ (appEndo . getHsType) finish r

  bind _ = pure ()

  order = pure ()

  uplo _ _ _ = pure ()

  trans _ _ _ = pure ()

  diag _ _ _ = pure ()

  dim _ _ _ = do
    n <- lift $ newName "n"
    let
      tvars = [ KindedTV n (ConT ''Nat) ]
      ctx = pure []
    tellHsType (forallT tvars ctx)
    lift (varT n)

  band _ _ _ = pure ()

  vec _ s n a = tellHsType $ \r -> [t| V $(varT s) $(pure n) $a -> $r |]

  scalar _ a = tellHsType $ \r -> [t| $a -> $r |]

  mat n _ _ _ _ s j k a =
    tellHsType $ \r ->
      [t| $(conT n) $(varT s) '($(pure j), $(pure k)) $a -> $r |]

  packMat n _ _ _ s j k a =
    tellHsType $ \r ->
      [t| $(conT n) $(varT s) '($(pure j), $(pure k)) $a -> $r |]

tellHsCall :: (Q Exp -> Q Exp) -> WriterT (Acc Exp) Q ()
tellHsCall = tell . (\f -> mempty { getCall = f }) . Dual . Endo

tellHsBody :: (Q Exp -> Q Exp) -> WriterT (Acc Exp) Q ()
tellHsBody = tell . (\f -> mempty { getBody = f }) . Endo

tellHsBind :: (Q Exp -> Q Exp) -> WriterT (Acc Exp) Q ()
tellHsBind = tell . (\f -> mempty { getBind = f }) . Endo

unsafeIOToST :: IO a -> ST s a
unsafeIOToST = unsafePrimToST

instance Build Exp where
  data Acc Exp
    = AccExp
    { getCall :: Dual (Endo (Q Exp))
    , getBody :: Endo (Q Exp)
    , getBind :: Endo (Q Exp)
    }
  type M Exp = ()
  type D Exp = ()
  type Nm Exp = Name

  build cnam go = do
    (a, finish) <- runWriterT (go ())
    let wrap = calling a id (\_body -> [| alloca $ \z -> $_body z >> peek z |])
        call = wrap ((appEndo . getDual . getCall) finish (varE cnam))
        body = (appEndo . getBody) finish call
    (appEndo . getBind) finish [| unsafeIOToST $body |]

  bind _nm = do
    _nm <- lift (newName _nm)
    tell mempty { getBind = Endo $ lam1E (varP _nm) }
    pure _nm

  order = tellHsCall $ \call -> [| $call 102 |]

  uplo nCon nField nExp = tellHsCall $ \call -> do
    up <- newName "up"
    [| case $(varE nExp) of
        $(recP nCon [ fieldPat nField (varP up) ]) ->
          $call (uploToI $(varE up))
     |]

  trans nCon nField nExp = tellHsCall $ \call -> do
    tr <- newName "tr"
    [| case $(varE nExp) of
        $(recP nCon [ fieldPat nField (varP tr) ]) ->
          $call (transToI $(varE tr))
     |]

  diag nCon nField nExp = tellHsCall $ \call -> do
    dg <- newName "dg"
    [| case $(varE nExp) of
        $(recP nCon [ fieldPat nField (varP dg) ]) ->
          $call (diagToI $(varE dg))
     |]

  dim nCon nField nExp =
    tellHsCall $ \call -> do
      p <- newName "p"
      [| case $(varE nExp) of
          $(recP nCon [ (,) nField <$> varP p ]) -> $call (fromN $(varE p))
       |]

  band nCon nField nExp =
    tellHsCall $ \call -> do
      p <- newName "p"
      [| case $(varE nExp) of
          $(recP nCon [ (,) nField <$> varP p ]) -> $call $(varE p)
       |]

  vec nExp _ _ _ = do
    p <- lift $ newName "p"
    i <- lift $ newName "i"
    tellHsCall $ \call -> [| $call $(varE p) $(varE i) |]
    tellHsBody $ \r -> do
      off <- newName "off"
      fp <- newName "fp"
      [| case $(varE nExp) of
           $(recP 'V
              [ fieldPat 'vptr (varP fp)
              , fieldPat 'voff (varP off)
              , fieldPat 'vinc (varP i)
              ]) ->
             withForeignPtr $(varE fp) $ \q ->
               $(lam1E (varP p) r) (advancePtr q (fromIntegral $(varE off)))
       |]

  scalar s a = do
    z <- lift $ newName "z"
    tellHsCall $ \call -> [| $call $(varE z) |]
    tellHsBody $ \r -> [| $(withE =<< a) $(varE s) $(lam1E (varP z) r) |]

  mat _ nCon nPtr nLead nExp _ _ _ _ = do
    p <- lift $ newName "p"
    i <- lift $ newName "i"
    tellHsCall $ \call -> [| $call $(varE p) $(varE i) |]
    tellHsBody $ \r -> do
      fp <- newName "fp"
      [| case $(varE nExp) of
           $(recP nCon [ fieldPat nPtr (varP fp), fieldPat nLead (varP i) ]) ->
             withForeignPtr $(varE fp) $(lam1E (varP p) r)
       |]

  packMat _ nCon nPtr nExp _ _ _ _ = do
    p <- lift $ newName "p"
    tellHsCall $ \call -> [| $call $(varE p) |]
    tellHsBody $ \r -> do
      fp <- newName "fp"
      [| case $(varE nExp) of
           $(recP nCon [ fieldPat nPtr (varP fp) ]) ->
             withForeignPtr $(varE fp) $(lam1E (varP p) r)
       |]

instance Semigroup (Acc Exp) where
  (<>) a b =
    AccExp
    { getCall = getCall a <> getCall b
    , getBody = getBody a <> getBody b
    , getBind = getBind a <> getBind b
    }

instance Monoid (Acc Exp) where
  mempty = AccExp { getCall = mempty, getBody = mempty, getBind = mempty }
  mappend = (<>)

matGE :: Build a => Nm a -> M a -> D a -> D a -> Q Type -> WriterT (Acc a) Q ()
matGE = mat ''GE 'GE 'geptr 'gelead

dimGE :: Build a => Nm a -> WriterT (Acc a) Q (D a, D a)
dimGE m = do
  trans 'GE 'getrans m
  (,) <$> dim 'GE 'gedim m <*> dim 'GE 'gecodim m

matGB :: Build a => Nm a -> M a -> D a -> D a -> Q Type -> WriterT (Acc a) Q ()
matGB = mat ''GB 'GB 'gbptr 'gblead

dimGB :: Build a => Nm a -> WriterT (Acc a) Q (D a, D a)
dimGB m = do
  trans 'GB 'gbtrans m
  n <- (,) <$> dim 'GB 'gbdim m <*> dim 'GB 'gbcodim m
  band 'GB 'gbsubdiag m
  band 'GB 'gbsuperdiag m
  pure n

matHE :: Build a => Nm a -> M a -> D a -> D a -> Q Type -> WriterT (Acc a) Q ()
matHE = mat ''HE 'HE 'heptr 'helead

dimHE :: Build a => Nm a -> WriterT (Acc a) Q (D a)
dimHE v = do
  uplo 'HE 'heuplo v
  dim 'HE 'hedim v

matHB :: Build a => Nm a -> M a -> D a -> D a -> Q Type -> WriterT (Acc a) Q ()
matHB = mat ''HB 'HB 'hbptr 'hblead

dimHB :: Build a => Nm a -> WriterT (Acc a) Q (D a)
dimHB v = do
  uplo 'HB 'hbuplo v
  n <- dim 'HB 'hbdim v
  band 'HB 'hboffdiag v
  pure n

matHP :: Build a => Nm a -> M a -> D a -> D a -> Q Type -> WriterT (Acc a) Q ()
matHP = packMat ''HP 'HP 'hpptr

dimHP :: Build a => Nm a -> WriterT (Acc a) Q (D a)
dimHP v = do
  uplo 'HP 'hpuplo v
  dim 'HP 'hpdim v

matTR :: Build a => Nm a -> M a -> D a -> D a -> Q Type -> WriterT (Acc a) Q ()
matTR = mat ''TR 'TR 'trptr 'trlead

dimTR :: Build a => Nm a -> WriterT (Acc a) Q (D a)
dimTR v = do
  uplo 'TR 'truplo v
  trans 'TR 'trtrans v
  diag 'TR 'trdiag v
  dim 'TR 'trdim v

matTB :: Build a => Nm a -> M a -> D a -> D a -> Q Type -> WriterT (Acc a) Q ()
matTB = mat ''TB 'TB 'tbptr 'tblead

dimTB :: Build a => Nm a -> WriterT (Acc a) Q (D a)
dimTB v = do
  uplo 'TB 'tbuplo v
  trans 'TB 'tbtrans v
  diag 'TB 'tbdiag v
  n <- dim 'TB 'tbdim v
  band 'TB 'tboffdiag v
  pure n

matTP :: Build a => Nm a -> M a -> D a -> D a -> Q Type -> WriterT (Acc a) Q ()
matTP = packMat ''TP 'TP 'tpptr

dimTP :: Build a => Nm a -> WriterT (Acc a) Q (D a)
dimTP v = do
  uplo 'TP 'tpuplo v
  trans 'TP 'tptrans v
  diag 'TP 'tpdiag v
  dim 'TP 'tpdim v

dimV :: Build a => Nm a -> WriterT (Acc a) Q (D a)
dimV = dim 'V 'vdim

withE :: Type -> Q Exp
withE a = calling a [| flip ($) |] [| with |]

allocaE :: Type -> Q Exp
allocaE a = calling a [| flip ($) () |] [| alloca |]

callE :: Type -> Q Exp
callE a = calling a [| \r _ -> r |] [| \f z -> f z >> peek z |]

calling :: Type -> a -> a -> a
calling t byVal byRef = do
  case t of
    AppT (ConT complex) _
      | complex == ''Complex -> byRef
    _ -> byVal

cblas_import :: String -> Q (C Type) -> Q (Name, Dec)
cblas_import fnam ctyp = do
  let
    cnam = "cblas_" ++ fnam
    hnam = mkName cnam
  (,) hnam <$> forImpD cCall unsafe cnam hnam (getC <$> ctyp)

cblas :: (forall a. Build a => M a -> WriterT (Acc a) Q Type)
      -> String
      -> Q [Dec]
cblas sig fnam = do
  (cnam, cdec) <- cblas_import fnam (build () sig)
  let hnam = mkName fnam
  body <- build cnam sig
  hsig <- sigD hnam (getHs <$> build () sig)
  hdec <- valD (varP hnam) (normalB $ pure body) []
  pure [cdec, hsig, hdec]

dot :: Build a => Q Type -> M a -> WriterT (Acc a) Q Type
dot t m = do
  x <- bind "x"
  y <- bind "y"
  n <- dimV x
  vec x m n t
  vec y m n t
  lift t

asum :: Build a => Q Type -> Q Type -> M a -> WriterT (Acc a) Q Type
asum r t m = do
  x <- bind "x"
  n <- dimV x
  vec x m n t
  lift r

unitT :: WriterT s Q Type
unitT = lift (tupleT 0)

swap :: Build a => Q Type -> M a -> WriterT (Acc a) Q Type
swap t m = do
  x <- bind "x"
  y <- bind "y"
  n <- dimV x
  vec x m n t
  vec y m n t
  unitT

copy :: Build a => Q Type -> M a -> WriterT (Acc a) Q Type
copy t m = do
  x <- bind "x"
  y <- bind "y"
  n <- dimV x
  vec x m n t
  vec y m n t
  unitT

axpy :: Build a => Q Type -> M a -> WriterT (Acc a) Q Type
axpy t m = do
  alpha <- bind "alpha"
  x <- bind "x"
  y <- bind "y"
  n <- dimV x
  scalar alpha t
  vec x m n t
  vec y m n t
  unitT

scal :: Build a => Q Type -> Q Type -> M a -> WriterT (Acc a) Q Type
scal a t m = do
  alpha <- bind "alpha"
  x <- bind "x"
  n <- dimV x
  scalar alpha a
  vec x m n t
  unitT

gemv :: Build a => Q Type -> M a -> WriterT (Acc a) Q Type
gemv t m = do
  alpha <- bind "alpha"
  a <- bind "a"
  x <- bind "x"
  beta <- bind "beta"
  y <- bind "y"
  order
  (r, c) <- dimGE a
  scalar alpha t
  matGE a m r c t
  vec x m c t
  scalar beta t
  vec y m r t
  unitT

ger :: Build a => Q Type -> M a -> WriterT (Acc a) Q Type
ger t m = do
  alpha <- bind "alpha"
  x <- bind "x"
  y <- bind "y"
  a <- bind "a"
  order
  j <- dimV x
  k <- dimV y
  scalar alpha t
  vec x m j t
  vec y m k t
  matGE a m j k t
  unitT

gbmv :: Build a => Q Type -> M a -> WriterT (Acc a) Q Type
gbmv t m = do
  alpha <- bind "alpha"
  a <- bind "a"
  x <- bind "x"
  beta <- bind "beta"
  y <- bind "y"
  order
  (r, c) <- dimGB a
  scalar alpha t
  matGB a m r c t
  vec x m c t
  scalar beta t
  vec y m r t
  unitT

hemv :: Build a => Q Type -> M a -> WriterT (Acc a) Q Type
hemv t m = do
  alpha <- bind "alpha"
  a <- bind "a"
  x <- bind "x"
  beta <- bind "beta"
  y <- bind "y"
  order
  n <- dimHE a
  scalar alpha t
  matHE a m n n t
  vec x m n t
  scalar beta t
  vec y m n t
  unitT

her :: Build a => Q Type -> Q Type -> M a -> WriterT (Acc a) Q Type
her s t m = do
  alpha <- bind "alpha"
  x <- bind "x"
  a <- bind "a"
  order
  n <- dimV x
  scalar alpha s
  vec x m n t
  matHE a m n n t
  unitT

her2 :: Build a => Q Type -> M a -> WriterT (Acc a) Q Type
her2 t m = do
  alpha <- bind "alpha"
  x <- bind "x"
  y <- bind "y"
  a <- bind "a"
  order
  n <- dimV x
  scalar alpha t
  vec x m n t
  vec y m n t
  matHE a m n n t
  unitT

hbmv :: Build a => Q Type -> M a -> WriterT (Acc a) Q Type
hbmv t m = do
  alpha <- bind "alpha"
  a <- bind "a"
  x <- bind "x"
  beta <- bind "beta"
  y <- bind "y"
  order
  n <- dimHB a
  scalar alpha t
  matHB a m n n t
  vec x m n t
  scalar beta t
  vec y m n t
  unitT

hpmv :: Build a => Q Type -> M a -> WriterT (Acc a) Q Type
hpmv t m = do
  alpha <- bind "alpha"
  a <- bind "a"
  x <- bind "x"
  beta <- bind "beta"
  y <- bind "y"
  order
  n <- dimHP a
  scalar alpha t
  matHP a m n n t
  vec x m n t
  scalar beta t
  vec y m n t
  unitT

hpr :: Build a => Q Type -> Q Type -> M a -> WriterT (Acc a) Q Type
hpr s t m = do
  alpha <- bind "alpha"
  x <- bind "x"
  a <- bind "a"
  order
  n <- dimV x
  scalar alpha s
  vec x m n t
  matHP a m n n t
  unitT

hpr2 :: Build a => Q Type -> M a -> WriterT (Acc a) Q Type
hpr2 t m = do
  alpha <- bind "alpha"
  x <- bind "x"
  y <- bind "y"
  a <- bind "a"
  order
  n <- dimV x
  scalar alpha t
  vec x m n t
  vec y m n t
  matHP a m n n t
  unitT

trmv :: Build a => Q Type -> M a -> WriterT (Acc a) Q Type
trmv t m = do
  a <- bind "a"
  x <- bind "x"
  order
  n <- dimTR a
  matTR a m n n t
  vec x m n t
  unitT

tpmv :: Build a => Q Type -> M a -> WriterT (Acc a) Q Type
tpmv t m = do
  a <- bind "a"
  x <- bind "x"
  order
  n <- dimTP a
  matTP a m n n t
  vec x m n t
  unitT

tbmv :: Build a => Q Type -> M a -> WriterT (Acc a) Q Type
tbmv t m = do
  a <- bind "a"
  x <- bind "x"
  order
  n <- dimTB a
  matTB a m n n t
  vec x m n t
  unitT

cblas_dot :: Q Type -> String -> Q [Dec]
cblas_dot t = cblas (dot t)

cblas_asum :: Q Type -> Q Type -> String -> Q [Dec]
cblas_asum r t = cblas (asum r t)

cblas_swap :: Q Type -> String -> Q [Dec]
cblas_swap t = cblas (swap t)

cblas_copy :: Q Type -> String -> Q [Dec]
cblas_copy t = cblas (copy t)

cblas_axpy :: Q Type -> String -> Q [Dec]
cblas_axpy t = cblas (axpy t)

cblas_scal :: Q Type -> Q Type -> String -> Q [Dec]
cblas_scal s t = cblas (scal s t)

cblas_gemv :: Q Type -> String -> Q [Dec]
cblas_gemv t = cblas (gemv t)

cblas_ger :: Q Type -> String -> Q [Dec]
cblas_ger t = cblas (ger t)

cblas_gbmv :: Q Type -> String -> Q [Dec]
cblas_gbmv t = cblas (gbmv t)

cblas_hemv :: Q Type -> String -> Q [Dec]
cblas_hemv t = cblas (hemv t)

cblas_her :: Q Type -> Q Type -> String -> Q [Dec]
cblas_her s t = cblas (her s t)

cblas_her2 :: Q Type -> String -> Q [Dec]
cblas_her2 t = cblas (her2 t)

cblas_hbmv :: Q Type -> String -> Q [Dec]
cblas_hbmv t = cblas (hbmv t)

cblas_hpmv :: Q Type -> String -> Q [Dec]
cblas_hpmv t = cblas (hpmv t)

cblas_hpr :: Q Type -> Q Type -> String -> Q [Dec]
cblas_hpr s t = cblas (hpr s t)

cblas_hpr2 :: Q Type -> String -> Q [Dec]
cblas_hpr2 t = cblas (hpr2 t)

cblas_trmv :: Q Type -> String -> Q [Dec]
cblas_trmv t = cblas (trmv t)

cblas_tpmv :: Q Type -> String -> Q [Dec]
cblas_tpmv t = cblas (tpmv t)

cblas_tbmv :: Q Type -> String -> Q [Dec]
cblas_tbmv t = cblas (tbmv t)
