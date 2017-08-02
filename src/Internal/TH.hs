{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Internal.TH where

import Control.Monad.Primitive
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict
import Data.Complex
import Data.Functor.Identity
import Data.Monoid (Dual(..), Endo(..))
import Data.Semigroup (Semigroup(..))
import Foreign.C.Types (CSize)
import Foreign.Ptr (Ptr)
import Foreign.Marshal.Utils (with)
import Language.Haskell.TH
import Language.Haskell.TH.Syntax hiding (lift)

import Data.Dim
import Internal.Int
import Internal.Mut
import Internal.Vector

newtype EndoK m a = EndoK { getEndoK :: a -> m a }

instance Monad m => Semigroup (EndoK m a) where
  (<>) a b = EndoK (\r -> getEndoK a r >>= getEndoK b)

instance Monad m => Monoid (EndoK m a) where
  mempty = EndoK pure
  mappend = (<>)

type EndoQ a = Dual (EndoK Q a)

endoQ :: (a -> Q a) -> EndoQ a
endoQ = Dual . EndoK

newtype WriterT w m a = WriterT { unWriterT :: w -> m (a, w) }

runWriterT :: (Monad m, Monoid w) => WriterT w m a -> m (a, w)
runWriterT wt = unWriterT wt mempty

instance Functor m => Functor (WriterT w m) where
  fmap f wr = WriterT $ \_w -> fmap (\(a, _w) -> (f a, _w)) (unWriterT wr _w)

instance Monad m => Applicative (WriterT w m) where
  pure a = WriterT $ \w -> pure (a, w)
  (<*>) wf wa  = WriterT $ \_w -> do
    (f, _w) <- unWriterT wf _w
    (a, _w) <- unWriterT wa _w
    pure (f a, _w)

instance Monad m => Monad (WriterT w m) where
  (>>=) wa kab = WriterT $ \_w -> do
    (a, _w) <- unWriterT wa _w
    unWriterT (kab a) _w

instance MonadTrans (WriterT w) where
  lift ma = WriterT $ \w -> fmap (\a -> (a, w)) ma

writerT :: (Monad m, Semigroup w) => m (a, w) -> WriterT w m a
writerT maw = WriterT $ \w0 -> do
  (a, w1) <- maw
  let w2 = w0 <> w1
  w2 `seq` pure (a, w2)

writer :: (Monad m, Semigroup w) => (a, w) -> WriterT w m a
writer aw = writerT (pure aw)

tell :: (Monad m, Semigroup w) => w -> WriterT w m ()
tell w = writer ((), w)

data TypeBuilder m d
  = TypeBuilder
  { build :: (m -> WriterT (EndoQ Type) Q Type) -> Q Type
  , dim :: WriterT (EndoQ Type) Q d
  , vec :: d -> Type -> WriterT (EndoQ Type) Q ()
  , mutvec :: m -> d -> Type -> WriterT (EndoQ Type) Q ()
  , scalar :: Type -> WriterT (EndoQ Type) Q ()
  }

calling :: Type -> a -> a -> a
calling t byValue byRef = do
  case t of
    AppT (ConT complex) _
      | complex == ''Complex -> byRef
    _ -> byValue

cTypeBuilder :: TypeBuilder () ()
cTypeBuilder =
  TypeBuilder
  { build = \go -> do
      (a, Dual (EndoK finish)) <- runWriterT (go ())
      r <- calling a [t| IO $(pure a) |] [t| Ptr $(pure a) -> IO () |]
      finish r
  , dim = dimC
  , vec = vecC
  , mutvec = mutvecC
  , scalar = scalarC
  }

dimC :: WriterT (EndoQ Type) Q ()
dimC =
  (tell . endoQ) $ \r -> [t| I -> $(pure r) |]

vecC :: () -> Type -> WriterT (EndoQ Type) Q ()
vecC () a =
  (tell . endoQ) $ \r -> [t| Ptr $(pure a) -> I -> $(pure r) |]

mutvecC :: () -> () -> Type -> WriterT (EndoQ Type) Q ()
mutvecC () = vecC

scalarC :: Type -> WriterT (EndoQ Type) Q ()
scalarC a = calling a byValue byRef
  where
    byValue = (tell . endoQ) $ \r -> [t| $(pure a) -> $(pure r) |]
    byRef = (tell . endoQ) $ \r -> [t| Ptr $(pure a) -> $(pure r) |]

dimHs :: WriterT (EndoQ Type) Q Type
dimHs = do
  n <- lift $ newName "n"
  nk <- lift [t| Dim |]
  let
    tvars = [ KindedTV n nk ]
    ctx = pure []
  tv <- lift $ varT n
  writer (tv, endoQ (\r -> forallT tvars ctx (pure r)))

vecHs :: Type -> Type -> WriterT (EndoQ Type) Q ()
vecHs n a =
  (tell . endoQ) $ \r -> [t| V $(pure n) $(pure a) -> $(pure r) |]

mutvecHs :: Type -> Type -> Type -> WriterT (EndoQ Type) Q ()
mutvecHs m n a =
  (tell . endoQ) $ \r ->
    [t| Mut (V $(pure n)) (PrimState $(pure m)) $(pure a) -> $(pure r) |]

scalarHs :: Type -> WriterT (EndoQ Type) Q ()
scalarHs a =
  (tell . endoQ) $ \r -> [t| $(pure a) -> $(pure r) |]

hsTypeBuilder :: TypeBuilder Type Type
hsTypeBuilder =
  TypeBuilder
  { build = \go -> do
      m <- newName "m"
      tv <- varT m
      (a, Dual (EndoK finish)) <- runWriterT (go tv)
      let
        tvars = [ KindedTV m (AppT (AppT ArrowT StarT) StarT) ]
        ctx = (:[]) <$> [t| PrimMonad $(pure tv) |]
      forallT tvars ctx (finish =<< [t| $(pure tv) $(pure a) |])
  , dim = dimHs
  , vec = vecHs
  , mutvec = mutvecHs
  , scalar = scalarHs
  }

cblas_import :: String -> Q Type -> Q (Name, Dec)
cblas_import fnam ctyp = do
  let
    cnam = "cblas_" ++ fnam
    hnam = mkName cnam
  (,) hnam <$> forImpD cCall unsafe cnam hnam ctyp

cblas :: (forall m d. TypeBuilder m d -> Q Type)
      -> String
      -> (Q Exp -> Q Exp)
      -> Q [Dec]
cblas sig fnam body = do
  (cnam, cdec) <- cblas_import fnam (sig cTypeBuilder)
  let hnam = mkName fnam
  hsig <- sigD hnam (sig hsTypeBuilder)
  hdec <- valD (varP hnam) (normalB $ body $ varE cnam) []
  pure [cdec, hsig, hdec]

dot :: Type -> TypeBuilder m d -> Q Type
dot t (TypeBuilder {..}) =
  build $ \_ -> do
    n <- dim
    vec n t
    vec n t
    return t

asum :: Type -> Type -> TypeBuilder m d -> Q Type
asum r t (TypeBuilder {..}) =
  build $ \_ -> do
    n <- dim
    vec n t
    return r

unitT :: Type
unitT = TupleT 0

swap :: Type -> TypeBuilder m d -> Q Type
swap t (TypeBuilder {..}) =
  build $ \m -> do
    n <- dim
    mutvec m n t
    mutvec m n t
    return unitT

copy :: Type -> TypeBuilder m d -> Q Type
copy t (TypeBuilder {..}) =
  build $ \m -> do
    n <- dim
    vec n t
    mutvec m n t
    return unitT

axpy :: Type -> TypeBuilder m d -> Q Type
axpy t (TypeBuilder {..}) =
  build $ \m -> do
    n <- dim
    scalar t
    vec n t
    mutvec m n t
    return unitT

scal :: Type -> Type -> TypeBuilder m d -> Q Type
scal a t (TypeBuilder {..}) =
  build $ \m -> do
    n <- dim
    scalar a
    mutvec m n t
    return unitT

withE :: Type -> Q Exp
withE a = calling a [| flip ($) |] [| with |]

allocaE :: Type -> Q Exp
allocaE a = calling a [| flip ($) () |] [| alloca |]

callE :: Type -> Q Exp
callE a = calling a [| \r _ -> r |] [| \f z -> f z >> peek z |]

cblas_dot :: Q Type -> String -> Q [Dec]
cblas_dot t fnam = do
  _t <- t
  cblas (dot _t) fnam $ \cfun ->
    [|
      \x y ->
        unsafePrimToPrim $
        $(allocaE _t) $ \z ->
        unsafeWithV x $ \nn px incx ->
        unsafeWithV y $ \_ py incy ->
          $(callE _t) ($cfun nn px incx py incy) z
    |]

cblas_asum :: Q Type -> Q Type -> String -> Q [Dec]
cblas_asum r t fnam = do
  _r <- r
  _t <- t
  cblas (asum _r _t) fnam $ \cfun ->
    [|
      \x ->
        unsafePrimToPrim $
        unsafeWithV x $ \nn px incx ->
          $cfun nn px incx
    |]

cblas_swap :: Q Type -> String -> Q [Dec]
cblas_swap t fnam = do
  _t <- t
  cblas (swap _t) fnam $ \cfun ->
    [|
      \x y ->
        unsafePrimToPrim $
        withV x $ \nn px incx ->
        withV y $ \_ py incy ->
          $cfun nn px incx py incy
    |]

cblas_copy :: Q Type -> String -> Q [Dec]
cblas_copy t fnam = do
  _t <- t
  cblas (copy _t) fnam $ \cfun ->
    [|
      \x y ->
        unsafePrimToPrim $
        unsafeWithV x $ \nn px incx ->
        withV y $ \_ py incy ->
          $cfun nn px incx py incy
    |]

cblas_axpy :: Q Type -> String -> Q [Dec]
cblas_axpy _t fnam = do
  _t <- _t
  cblas (axpy _t) fnam $ \cfun -> do
    [|
      \a x y ->
        unsafePrimToPrim $
        $(withE _t) a $ \pa ->
        unsafeWithV x $ \nn px incx ->
        withV y $ \_ py incy ->
          $cfun nn pa px incx py incy
     |]

cblas_scal :: Q Type -> Q Type -> String -> Q [Dec]
cblas_scal _s _t fnam = do
  _s <- _s
  _t <- _t
  cblas (scal _s _t) fnam $ \cfun ->
    [|
      \a x ->
        unsafePrimToPrim $
        $(withE _s) a $ \pa ->
        withV x $ \nn px incx ->
          $cfun nn pa px incx
     |]

{-

gemv :: Q Type -> TypeBuilder env () -> Q Type
gemv t (TypeBuilder {..}) =
  finish () $ order . ge . scalar t . mat t . vec t . scalar t . mutvec t
-}
