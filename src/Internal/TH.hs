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
  dim :: E a -> WriterT (Acc a) Q (D a)
  mutdim :: E a -> WriterT (Acc a) Q (D a)
  vec :: E a -> D a -> Q Type -> WriterT (Acc a) Q ()
  mutvec :: E a -> M a -> D a -> Q Type -> WriterT (Acc a) Q ()
  scalar :: E a -> Q Type -> WriterT (Acc a) Q ()

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

  dim _ = (tell . AccC . Dual . Endo) $ \r -> [t| I -> $(pure r) |]
  mutdim = dim

  vec _ () a =
    (tell . AccC . Dual . Endo) $ \r -> [t| Ptr $a -> I -> $(pure r) |]
  mutvec e _ = vec e

  scalar _ _a = lift _a >>= \_a -> calling _a byValue byRef
    where
      byValue =
        (tell . AccC . Dual . Endo) $ \r -> [t| $_a -> $(pure r) |]
      byRef =
        (tell . AccC . Dual . Endo) $ \r -> [t| Ptr $_a -> $(pure r) |]

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

  dim _ = do
    n <- lift $ newName "n"
    nk <- lift [t| Dim |]
    let
      tvars = [ KindedTV n nk ]
      ctx = pure []
    tv <- lift $ varT n
    (tell . AccHsType . Dual . Endo) $ \r -> forallT tvars ctx (pure r)
    pure tv

  mutdim = dim

  vec _ n a =
    (tell . AccHsType . Dual . Endo) $ \r ->
      [t| V $(pure n) $a -> $(pure r) |]

  mutvec _ m n a =
    (tell . AccHsType . Dual . Endo) $ \r ->
      [t| Mut (V $(pure n)) (PrimState $(pure m)) $a -> $(pure r) |]

  scalar _ a =
    (tell . AccHsType . Dual . Endo) $ \r -> [t| $a -> $(pure r) |]

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

  dim v = do
    nn <- lift $ newName "nn"
    tell mempty
      { getCall = Endo $ \call -> [| $(pure call) $(varE nn) |]
      , getBody = (Dual . Endo) $ \r ->
          [| unsafeWithV $(pure v) $(lamE [varP nn, wildP, wildP] (pure r)) |]
      }

  mutdim v = do
    nn <- lift $ newName "nn"
    tell mempty
      { getCall = Endo $ \call -> [| $(pure call) $(varE nn) |]
      , getBody = (Dual . Endo) $ \r ->
          [| withV $(pure v) $(lamE [varP nn, wildP, wildP] (pure r)) |]
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

  mutvec v _ _ _ = do
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
  n <- dim x
  vec x n t
  vec y n t
  lift t

asum :: Build a => Q Type -> Q Type -> M a -> WriterT (Acc a) Q Type
asum r t _ = do
  x <- bind "x"
  n <- dim x
  vec x n t
  lift r

unitT :: WriterT s Q Type
unitT = lift (tupleT 0)

swap :: Build a => Q Type -> M a -> WriterT (Acc a) Q Type
swap t m = do
  x <- bind "x"
  y <- bind "y"
  n <- mutdim x
  mutvec x m n t
  mutvec y m n t
  unitT

copy :: Build a => Q Type -> M a -> WriterT (Acc a) Q Type
copy t m = do
  x <- bind "x"
  y <- bind "y"
  n <- dim x
  vec x n t
  mutvec y m n t
  unitT

axpy :: Build a => Q Type -> M a -> WriterT (Acc a) Q Type
axpy t m = do
  alpha <- bind "alpha"
  x <- bind "x"
  y <- bind "y"
  n <- dim x
  scalar alpha t
  vec x n t
  mutvec y m n t
  unitT

scal :: Build a => Q Type -> Q Type -> M a -> WriterT (Acc a) Q Type
scal a t m = do
  alpha <- bind "alpha"
  x <- bind "x"
  n <- mutdim x
  scalar alpha a
  mutvec x m n t
  unitT

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
