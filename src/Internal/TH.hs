{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Internal.TH where

import Control.Monad.Primitive
import Control.Monad.Trans.Class
import Data.Complex
import Data.Monoid (Dual(..))
import Data.Semigroup (Semigroup(..))
import Foreign.Ptr (Ptr)
import Foreign.Marshal.Utils (with)
import Language.Haskell.TH

import Data.Dim
import Internal.Endo
import Internal.Int (I)
import Internal.Matrix
import Internal.Mut
import Internal.Vector
import Internal.Writer


newtype C a = C { getC :: a }

newtype Hs a = Hs { getHs :: a }

newtype HsExp = HsExp { getHsExp :: Exp }

class Build a where
  data Acc a
  type M a
  type D a
  type E a
  build :: Monoid (Acc a) => E a -> (M a -> WriterT (Acc a) Q Type) -> Q a
  bind :: String -> WriterT (Acc a) Q (E a)
  order :: WriterT (Acc a) Q ()
  dimVec :: E a -> WriterT (Acc a) Q (D a)
  dimMutVec :: E a -> WriterT (Acc a) Q (D a)
  dimGE :: E a -> WriterT (Acc a) Q (D a, D a)
  dimGB :: E a -> WriterT (Acc a) Q (D a, D a)
  dimHE :: E a -> WriterT (Acc a) Q (D a)
  vec :: E a -> D a -> Q Type -> WriterT (Acc a) Q ()
  mutVec :: E a -> M a -> D a -> Q Type -> WriterT (Acc a) Q ()
  scalar :: E a -> Q Type -> WriterT (Acc a) Q ()
  matGE :: E a -> D a -> D a -> Q Type -> WriterT (Acc a) Q ()
  matMutGE :: E a -> M a -> D a -> D a -> Q Type -> WriterT (Acc a) Q ()
  matGB :: E a -> D a -> D a -> Q Type -> WriterT (Acc a) Q ()
  matHE :: E a -> D a -> Q Type -> WriterT (Acc a) Q ()
  matMutHE :: E a -> M a -> D a -> Q Type -> WriterT (Acc a) Q ()

tellC :: (Type -> Q Type) -> WriterT (Acc (C Type)) Q ()
tellC = tell . AccC . Dual . Endo

instance Build (C Type) where
  newtype Acc (C Type) = AccC { getType :: Dual (Endo Q Type) }
    deriving (Monoid, Semigroup)
  type M (C Type) = ()
  type D (C Type) = ()
  type E (C Type) = ()

  build _ go = do
    (a, finish) <- runWriterT (go ())
    r <- calling a [t| IO $(pure a) |] [t| Ptr $(pure a) -> IO () |]
    C <$> (getEndo . getDual . getType) finish r

  bind _ = pure ()

  order = tellC $ \r -> [t| I -> $(pure r) |]

  dimVec _ = tellC $ \r -> [t| I -> $(pure r) |]
  dimMutVec = dimVec

  dimGE _ = do
    tellC $ \r -> [t| I -> I -> I -> $(pure r) |]
    pure ((), ())

  dimGB _ = do
    tellC $ \r -> [t| I -> I -> I -> I -> I -> $(pure r) |]
    pure ((), ())

  dimHE _ = tellC $ \r -> [t| I -> I -> $(pure r) |]

  vec _ () a = tellC $ \r -> [t| Ptr $a -> I -> $(pure r) |]
  mutVec e _ = vec e

  scalar _ _a = lift _a >>= \_a -> calling _a byValue byRef
    where
      byValue = tellC $ \r -> [t| $_a -> $(pure r) |]
      byRef = tellC $ \r -> [t| Ptr $_a -> $(pure r) |]

  matGE _ _ _ a = tellC $ \r -> [t| Ptr $a -> I -> $(pure r) |]

  matMutGE e _ = matGE e

  matGB _ _ _ a = tellC $ \r -> [t| Ptr $a -> I -> $(pure r) |]

  matHE _ _ a = tellC $ \r -> [t| Ptr $a -> I -> $(pure r) |]

  matMutHE e _ = matHE e

tellHsType :: (Type -> Q Type) -> WriterT (Acc (Hs Type)) Q ()
tellHsType = tell . AccHsType . Dual . Endo

instance Build (Hs Type) where
  newtype Acc (Hs Type) = AccHsType { getHsType :: Dual (Endo Q Type) }
    deriving (Monoid, Semigroup)
  type M (Hs Type) = Type
  type D (Hs Type) = Type
  type E (Hs Type) = ()

  build _ go = do
    m <- newName "m"
    tv <- varT m
    (a, finish) <- runWriterT (go tv)
    let
      tvars = [ KindedTV m (AppT (AppT ArrowT StarT) StarT) ]
      ctx = (:[]) <$> [t| PrimMonad $(pure tv) |]
    r <- [t| $(pure tv) $(pure a) |]
    (<$>) Hs $ forallT tvars ctx $ (getEndo . getDual . getHsType) finish r

  bind _ = pure ()

  order = pure ()

  dimVec _ = do
    n <- lift $ newName "n"
    dimk <- lift [t| Dim |]
    let
      tvars = [ KindedTV n dimk ]
      ctx = pure []
    tellHsType $ \r -> forallT tvars ctx (pure r)
    lift (varT n)

  dimMutVec = dimVec

  dimGE _ = do
    j <- lift $ newName "j"
    k <- lift $ newName "k"
    dimk <- lift [t| Dim |]
    let
      tvars = [ KindedTV j dimk, KindedTV k dimk ]
      ctx = pure []
    tellHsType $ \r -> forallT tvars ctx (pure r)
    lift $ (,) <$> varT j <*> varT k

  dimGB _ = do
    j <- lift $ newName "j"
    k <- lift $ newName "k"
    dimk <- lift [t| Dim |]
    let
      tvars = [ KindedTV j dimk, KindedTV k dimk ]
      ctx = pure []
    tellHsType $ \r -> forallT tvars ctx (pure r)
    lift $ (,) <$> varT j <*> varT k

  dimHE _ = do
    n <- lift $ newName "n"
    dimk <- lift [t| Dim |]
    let
      tvars = [ KindedTV n dimk ]
      ctx = pure []
    tellHsType $ \r -> forallT tvars ctx (pure r)
    lift (varT n)

  vec _ n a = tellHsType $ \r -> [t| V $(pure n) $a -> $(pure r) |]

  mutVec _ m n a =
    tellHsType $ \r ->
      [t| Mut (V $(pure n)) (PrimState $(pure m)) $a -> $(pure r) |]

  scalar _ a = tellHsType $ \r -> [t| $a -> $(pure r) |]

  matGE _ j k a =
    tellHsType $ \r -> [t| GE $(pure j) $(pure k) $a -> $(pure r) |]

  matMutGE _ m j k a =
    tellHsType $ \r ->
      [t| Mut (GE $(pure j) $(pure k)) (PrimState $(pure m)) $a -> $(pure r) |]

  matGB _ j k a =
    tellHsType $ \r -> [t| GB $(pure j) $(pure k) $a -> $(pure r) |]

  matHE _ n a = tellHsType $ \r -> [t| HE $(pure n) $a -> $(pure r) |]

  matMutHE _ m n a = tellHsType $ \r ->
    [t| Mut (HE $(pure n)) (PrimState $(pure m)) $a -> $(pure r) |]

instance Build Exp where
  data Acc Exp
    = AccExp
    { getCall :: Endo Q Exp
    , getBody :: Dual (Endo Q Exp)
    , getBind :: Dual (Endo Q Exp)
    }
  type M Exp = ()
  type D Exp = ()
  type E Exp = Exp

  build call go = do
    (a, finish) <- runWriterT (go ())
    _body <- (getEndo . getCall) finish call
    let wrap = calling a pure (\b -> [| alloca $ \z -> $(pure b) z >> peek z |])
    _body <- wrap _body
    _body <- (getEndo . getDual . getBody) finish _body
    _body <- [| unsafePrimToPrim $(pure _body) |]
    (getEndo . getDual . getBind) finish _body

  bind _nm = do
    _nm <- lift (newName _nm)
    tell mempty { getBind = (Dual . Endo) $ lam1E (varP _nm) . pure }
    lift (varE _nm)

  order = tell mempty { getCall = Endo $ \call -> [| $(pure call) 102 |] }

  dimVec v = do
    nn <- lift $ newName "nn"
    tell mempty
      { getCall = Endo $ \call -> [| $(pure call) $(varE nn) |]
      , getBody = (Dual . Endo) $ \r ->
          [| unsafeWithV $(pure v) $(lamE [varP nn, wildP, wildP] (pure r)) |]
      }

  dimMutVec v = do
    nn <- lift $ newName "nn"
    tell mempty
      { getCall = Endo $ \call -> [| $(pure call) $(varE nn) |]
      , getBody = (Dual . Endo) $ \r ->
          [| withV $(pure v) $(lamE [varP nn, wildP, wildP] (pure r)) |]
      }

  dimGE m = do
    t <- lift (newName "t")
    r <- lift (newName "r")
    c <- lift (newName "c")
    tell mempty
      { getCall = Endo $ \call ->
          [| $(pure call) (transToI $(varE t)) $(varE r) $(varE c) |]
      , getBody = (Dual . Endo) $ \rest ->
          [| unsafeWithGE $(pure m)
             $(lamE [varP t, varP r, varP c, wildP, wildP] (pure rest)) |]
      }
    pure ((), ())

  dimGB m = do
    t <- lift (newName "t")
    r <- lift (newName "r")
    c <- lift (newName "c")
    kl <- lift (newName "kl")
    ku <- lift (newName "ku")
    tell mempty
      { getCall = Endo $ \call ->
          [| $(pure call)
             (transToI $(varE t)) $(varE r) $(varE c)
             $(varE kl) $(varE ku) |]
      , getBody = (Dual . Endo) $ \rest ->
          let binds = [varP t, varP r, varP c, varP kl, varP ku, wildP, wildP] in
            [| unsafeWithGB $(pure m) $(lamE binds (pure rest)) |]
      }
    pure ((), ())

  dimHE v = do
    uplo <- lift $ newName "uplo"
    nn <- lift $ newName "nn"
    tell mempty
      { getCall = Endo $ \call ->
          [| $(pure call) (uploToI $(varE uplo)) $(varE nn) |]
      , getBody = (Dual . Endo) $ \r ->
          let binds = [varP uplo, varP nn, wildP, wildP] in
            [| unsafeWithHE $(pure v) $(lamE binds (pure r)) |]
      }

  vec v _ _ = do
    p <- lift $ newName "p"
    i <- lift $ newName "i"
    tell mempty
          { getCall = Endo $ \call -> [| $(pure call) $(varE p) $(varE i) |]
          , getBody = (Dual . Endo) $ \r ->
              [| unsafeWithV $(pure v)
                 $(lamE [wildP, varP p, varP i] (pure r)) |]
          }

  mutVec v _ _ _ = do
    p <- lift $ newName "p"
    i <- lift $ newName "i"
    tell mempty
          { getCall = Endo $ \call -> [| $(pure call) $(varE p) $(varE i) |]
          , getBody = (Dual . Endo) $ \r ->
              [| withV $(pure v) $(lamE [wildP, varP p, varP i] (pure r)) |]
          }

  scalar s a = do
    z <- lift $ newName "z"
    tell mempty
      { getCall = Endo $ \call -> [| $(pure call) $(varE z) |]
      , getBody = (Dual . Endo) $ \r ->
          [| $(withE =<< a) $(pure s) $(lam1E (varP z) (pure r)) |]
      }

  matGE gb _ _ _ = do
    p <- lift (newName "p")
    ld <- lift (newName "ld")
    tell mempty
          { getCall = Endo $ \call -> [| $(pure call) $(varE p) $(varE ld) |]
          , getBody = (Dual . Endo) $ \r ->
              [| unsafeWithGB $(pure gb)
                 $(lamE [wildP, wildP, wildP, varP p, varP ld] (pure r)) |]
          }

  matMutGE ge _ _ _ _ = do
    p <- lift (newName "p")
    ld <- lift (newName "ld")
    tell mempty
          { getCall = Endo $ \call -> [| $(pure call) $(varE p) $(varE ld) |]
          , getBody = (Dual . Endo) $ \r ->
              let binds = [wildP, wildP, wildP, varP p, varP ld] in
                [| withGE $(pure ge) $(lamE binds (pure r)) |]
          }

  matGB gb _ _ _ = do
    p <- lift (newName "p")
    ld <- lift (newName "ld")
    tell mempty
          { getCall = Endo $ \call -> [| $(pure call) $(varE p) $(varE ld) |]
          , getBody = (Dual . Endo) $ \r ->
              let binds = [wildP, wildP, wildP, wildP, wildP, varP p, varP ld] in
                [| unsafeWithGB $(pure gb) $(lamE binds (pure r)) |]
          }

  matHE he _ _ = do
    p <- lift (newName "p")
    ld <- lift (newName "ld")
    tell mempty
          { getCall = Endo $ \call -> [| $(pure call) $(varE p) $(varE ld) |]
          , getBody = (Dual . Endo) $ \r ->
              let binds = [wildP, wildP, varP p, varP ld] in
                [| unsafeWithHE $(pure he) $(lamE binds (pure r)) |]
          }

  matMutHE he _ _ _ = do
    p <- lift (newName "p")
    ld <- lift (newName "ld")
    tell mempty
          { getCall = Endo $ \call -> [| $(pure call) $(varE p) $(varE ld) |]
          , getBody = (Dual . Endo) $ \r ->
              let binds = [wildP, wildP, varP p, varP ld] in
                [| withHE $(pure he) $(lamE binds (pure r)) |]
          }

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

cblas_import :: String -> Q (C Type) -> Q (Exp, Dec)
cblas_import fnam ctyp = do
  let
    cnam = "cblas_" ++ fnam
    hnam = mkName cnam
  (,) <$> varE hnam <*> forImpD cCall unsafe cnam hnam (getC <$> ctyp)

cblas :: (forall a. Build a => M a -> WriterT (Acc a) Q Type)
      -> String
      -> Q [Dec]
cblas sig fnam = do
  (call, cdec) <- cblas_import fnam (build () sig)
  let hnam = mkName fnam
  body <- build call sig
  hsig <- sigD hnam (getHs <$> build () sig)
  hdec <- valD (varP hnam) (normalB $ pure body) []
  pure [cdec, hsig, hdec]

dot :: Build a => Q Type -> M a -> WriterT (Acc a) Q Type
dot t _ = do
  x <- bind "x"
  y <- bind "y"
  n <- dimVec x
  vec x n t
  vec y n t
  lift t

asum :: Build a => Q Type -> Q Type -> M a -> WriterT (Acc a) Q Type
asum r t _ = do
  x <- bind "x"
  n <- dimVec x
  vec x n t
  lift r

unitT :: WriterT s Q Type
unitT = lift (tupleT 0)

swap :: Build a => Q Type -> M a -> WriterT (Acc a) Q Type
swap t m = do
  x <- bind "x"
  y <- bind "y"
  n <- dimMutVec x
  mutVec x m n t
  mutVec y m n t
  unitT

copy :: Build a => Q Type -> M a -> WriterT (Acc a) Q Type
copy t m = do
  x <- bind "x"
  y <- bind "y"
  n <- dimVec x
  vec x n t
  mutVec y m n t
  unitT

axpy :: Build a => Q Type -> M a -> WriterT (Acc a) Q Type
axpy t m = do
  alpha <- bind "alpha"
  x <- bind "x"
  y <- bind "y"
  n <- dimVec x
  scalar alpha t
  vec x n t
  mutVec y m n t
  unitT

scal :: Build a => Q Type -> Q Type -> M a -> WriterT (Acc a) Q Type
scal a t m = do
  alpha <- bind "alpha"
  x <- bind "x"
  n <- dimMutVec x
  scalar alpha a
  mutVec x m n t
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
  matGE a r c t
  vec x c t
  scalar beta t
  mutVec y m r t
  unitT

ger :: Build a => Q Type -> M a -> WriterT (Acc a) Q Type
ger t m = do
  alpha <- bind "alpha"
  x <- bind "x"
  y <- bind "y"
  a <- bind "a"
  order
  j <- dimVec x
  k <- dimVec y
  scalar alpha t
  vec x j t
  vec y k t
  matMutGE a m j k t
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
  matGB a r c t
  vec x c t
  scalar beta t
  mutVec y m r t
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
  matHE a n t
  vec x n t
  scalar beta t
  mutVec y m n t
  unitT

her :: Build a => Q Type -> Q Type -> M a -> WriterT (Acc a) Q Type
her s t m = do
  alpha <- bind "alpha"
  x <- bind "x"
  a <- bind "a"
  order
  n <- dimVec x
  scalar alpha s
  vec x n t
  matMutHE a m n t
  unitT

her2 :: Build a => Q Type -> M a -> WriterT (Acc a) Q Type
her2 t m = do
  alpha <- bind "alpha"
  x <- bind "x"
  y <- bind "y"
  a <- bind "a"
  order
  n <- dimVec x
  scalar alpha t
  vec x n t
  vec y n t
  matMutHE a m n t
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
