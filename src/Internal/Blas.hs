{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Internal.Blas where

import Control.Monad.Primitive
import Data.Complex
import Foreign.C.Types (CSize(..))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Storable (Storable, peek)
import Prelude hiding (length)

import Internal.Int
import Internal.Matrix
import Internal.Mut
import Internal.TH
import Internal.Vector

cblas_dot [t| Float |] "sdot"
cblas_dot [t| Double |] "ddot"
cblas_dot [t| Complex Float |] "cdot"
cblas_dot [t| Complex Double |] "zdot"

cblas_asum [t| Float |] [t| Float |] "snrm2"
cblas_asum [t| Float |] [t| Float |] "sasum"
cblas_asum [t| CSize |] [t| Float |] "isamax"

cblas_asum [t| Double |] [t| Double |] "dnrm2"
cblas_asum [t| Double |] [t| Double |] "dasum"
cblas_asum [t| CSize |] [t| Double |] "idamax"

cblas_asum [t| Float |] [t| Complex Float |] "scnrm2"
cblas_asum [t| Float |] [t| Complex Float |] "scasum"
cblas_asum [t| CSize |] [t| Complex Float |] "icamax"

cblas_asum [t| Double |] [t| Complex Double |] "dznrm2"
cblas_asum [t| Double |] [t| Complex Double |] "dzasum"
cblas_asum [t| CSize |] [t| Complex Double |] "izamax"

cblas_swap [t| Float |] "sswap"
cblas_swap [t| Double |] "dswap"
cblas_swap [t| Complex Float |] "cswap"
cblas_swap [t| Complex Double |] "zswap"

cblas_copy [t| Float |] "scopy"
cblas_copy [t| Double |] "dcopy"
cblas_copy [t| Complex Float |] "ccopy"
cblas_copy [t| Complex Double |] "zcopy"

cblas_axpy [t| Float |] "saxpy"
cblas_axpy [t| Double |] "daxpy"
cblas_axpy [t| Complex Float |] "caxpy"
cblas_axpy [t| Complex Double |] "zaxpy"

cblas_scal [t| Float |] [t| Float |] "sscal"
cblas_scal [t| Double |] [t| Double |] "dscal"
cblas_scal [t| Complex Float |] [t| Complex Float |] "cscal"
cblas_scal [t| Complex Double |] [t| Complex Double |] "zscal"
cblas_scal [t| Float |] [t| Complex Float |] "csscal"
cblas_scal [t| Double |] [t| Complex Double |] "zdscal"

cblas_gemv [t| Float |] "sgemv"
cblas_gemv [t| Double |] "dgemv"
cblas_gemv [t| Complex Float |] "cgemv"
cblas_gemv [t| Complex Double |] "zgemv"

cblas_ger [t| Float |] "sger"
cblas_ger [t| Double |] "dger"
cblas_ger [t| Complex Float |] "cgeru"
cblas_ger [t| Complex Float |] "cgerc"
cblas_ger [t| Complex Double |] "zgeru"
cblas_ger [t| Complex Double |] "zgerc"

cblas_gbmv [t| Float |] "sgbmv"
cblas_gbmv [t| Double |] "dgbmv"
cblas_gbmv [t| Complex Float |] "cgbmv"
cblas_gbmv [t| Complex Double |] "zgbmv"

cblas_hemv [t| Float |] "ssymv"
cblas_hemv [t| Double |] "dsymv"
cblas_hemv [t| Complex Float |] "chemv"
cblas_hemv [t| Complex Double |] "zhemv"

cblas_her [t| Float |] [t| Float |] "ssyr"
cblas_her [t| Double |] [t| Double |] "dsyr"
cblas_her [t| Float |] [t| Complex Float |] "cher"
cblas_her [t| Double |] [t| Complex Double |] "zher"

cblas_her2 [t| Float |] "ssyr2"
cblas_her2 [t| Double |] "dsyr2"
cblas_her2 [t| Complex Float |] "cher2"
cblas_her2 [t| Complex Double |] "zher2"

cblas_hbmv [t| Float |] "ssbmv"
cblas_hbmv [t| Double |] "dsbmv"
cblas_hbmv [t| Complex Float |] "chbmv"
cblas_hbmv [t| Complex Double |] "zhbmv"

cblas_hpmv [t| Float |] "sspmv"
cblas_hpmv [t| Double |] "dspmv"
cblas_hpmv [t| Complex Float |] "chpmv"
cblas_hpmv [t| Complex Double |] "zhpmv"

cblas_hpr [t| Float |] [t| Float |] "sspr"
cblas_hpr [t| Double |] [t| Double |] "dspr"
cblas_hpr [t| Float |] [t| Complex Float |] "chpr"
cblas_hpr [t| Double |] [t| Complex Double |] "zhpr"

cblas_hpr2 [t| Float |] "sspr2"
cblas_hpr2 [t| Double |] "dspr2"
cblas_hpr2 [t| Complex Float |] "chpr2"
cblas_hpr2 [t| Complex Double |] "zhpr2"

class Storable a => Scalar a where
  type RealPart a

  -- | Unconjugated inner (dot) product, @x^T x@.
  dotu :: PrimMonad m => V n a -> V n a -> m a

  -- | Conjugated inner (dot) product, @x^H x@.
  dotc :: PrimMonad m => V n a -> V n a -> m a

  -- | The sum of the real modulus of each element of the vector,
  -- @sum . map magnitude@.
  asum :: PrimMonad m => V n a -> m (RealPart a)

  -- | The Euclidean norm of the vector,
  -- @sqrt . sum . map (\x -> realPart (conjugate x * x))@
  nrm2 :: PrimMonad m => V n a -> m (RealPart a)

  -- | The index of the element of the vector with the largest real
  -- modulus.
  iamax :: PrimMonad m => V n a -> m CSize

  -- | @y <- a x + y@
  axpy
    :: PrimMonad m =>
       a  -- ^ scalar @a@
    -> V n a  -- ^ vector @x@
    -> Mut (V n) (PrimState m) a  -- ^ vector @y@
    -> m ()

  -- | @x <- a x@
  scal
    :: PrimMonad m =>
       a  -- ^ scalar @a@
    -> Mut (V n) (PrimState m) a  -- ^ vector @x@
    -> m ()

  -- | @x <- a x@
  rscal
    :: PrimMonad m =>
       RealPart a  -- ^ scalar @a@
    -> Mut (V n) (PrimState m) a  -- ^ vector @x@
    -> m ()

  -- | @y <- x@
  copy
    :: PrimMonad m =>
       V n a  -- ^ vector @x@
    -> Mut (V n) (PrimState m) a  -- ^ vector @y@
    -> m ()

  -- | @(x, y) <- (y, x)
  swap
    :: PrimMonad m =>
       Mut (V n) (PrimState m) a  -- ^ vector @x@
    -> Mut (V n) (PrimState m) a  -- ^ vector @y@
    -> m ()

  -- | @y <- alpha A x + beta y@
  gbmv
    :: PrimMonad m =>
       a  -- ^ scalar @alpha@
    -> GB j k a  -- ^ matrix @A@
    -> V k a  -- ^ vector @x@
    -> a  -- ^ scalar @beta@
    -> Mut (V j) (PrimState m) a  -- ^ vector @y@
    -> m ()

  -- | @y <- alpha A x + beta y@
  gemv
    :: PrimMonad m =>
       a  -- ^ scalar @alpha@
    -> GE j k a  -- ^ matrix @A@
    -> V k a  -- ^ vector @x@
    -> a  -- ^ scalar @beta@
    -> Mut (V j) (PrimState m) a  -- ^ vector @y@
    -> m ()

  -- | @y <- alpha A x + beta y@
  hbmv
    :: PrimMonad m =>
       a  -- ^ scalar @alpha@
    -> HB n a  -- ^ matrix @A@
    -> V n a  -- ^ vector @x@
    -> a  -- ^ scalar @beta@
    -> Mut (V n) (PrimState m) a  -- ^ vector @y@
    -> m ()

  -- | @y <- alpha A x + beta y@
  hemv
    :: PrimMonad m =>
       a
    -> HE n a
    -> V n a
    -> a
    -> Mut (V n) (PrimState m) a
    -> m ()

  -- | @y <- alpha A x + beta y@
  hpmv
    :: PrimMonad m =>
       a
    -> HP n a
    -> V n a
    -> a
    -> Mut (V n) (PrimState m) a
    -> m ()

  -- | @y <- alpha A x + beta y@
  tbmv
    :: PrimMonad m =>
       TB n a
    -> Mut (V k) (PrimState m) a
    -> m ()

  -- | @y <- alpha A x + beta y@
  tpmv
    :: PrimMonad m =>
       TP n a
    -> Mut (V k) (PrimState m) a
    -> m ()

  -- | @y <- alpha A x + beta y@
  trmv
    :: PrimMonad m =>
       TR n a
    -> Mut (V k) (PrimState m) a
    -> m ()

  -- | Compute the solution of a system of linear equations,
  -- @y <- x@ where @A x = y@.
  tbsv
    :: PrimMonad m =>
       TB n a
    -> Mut (V k) (PrimState m) a
    -> m ()

  -- | Compute the solution of a system of linear equations,
  -- @y <- x@ where @A x = y@.
  tpsv
    :: PrimMonad m =>
       TP n a
    -> Mut (V k) (PrimState m) a
    -> m ()

  -- | Compute the solution of a system of linear equations,
  -- @y <- x@ where @A x = y@.
  trsv
    :: PrimMonad m =>
       TR n a
    -> Mut (V k) (PrimState m) a
    -> m ()

  -- | @A <- alpha x y^T + A@
  geru
    :: PrimMonad m =>
       a
    -> V j a
    -> V k a
    -> Mut (GE j k) (PrimState m) a
    -> m ()

  -- | @A <- alpha x y^H + A@
  gerc
    :: PrimMonad m =>
       a
    -> V j a
    -> V k a
    -> Mut (GE j k) (PrimState m) a
    -> m ()

  -- | @A <- alpha x x^H + A@
  her
    :: PrimMonad m =>
       RealPart a
    -> V n a
    -> Mut (HE n) (PrimState m) a
    -> m ()

  -- | @A <- alpha x y^H + conj(alpha) y x^H + A@
  her2
    :: PrimMonad m =>
       a
    -> V n a
    -> V n a
    -> Mut (HE n) (PrimState m) a
    -> m ()

  -- | @A <- alpha x x^H + A@
  hpr
    :: PrimMonad m =>
       RealPart a
    -> V n a
    -> Mut (HP n) (PrimState m) a
    -> m ()

  -- | @A <- alpha x y^H + conj(alpha) y x^H + A@
  hpr2
    :: PrimMonad m =>
       a
    -> V n a
    -> V n a
    -> Mut (HP n) (PrimState m) a
    -> m ()

instance Scalar Float where
  type RealPart Float = Float
  dotu = sdot
  dotc = sdot
  asum = sasum
  nrm2 = snrm2
  iamax = isamax
  axpy = saxpy
  scal = sscal
  rscal = sscal
  copy = scopy
  swap = sswap
  gemv = sgemv
  geru = sger
  gerc = sger
  gbmv = sgbmv
  hemv = ssymv
  her = ssyr
  her2 = ssyr2
  hbmv = ssbmv
  hpmv = sspmv
  hpr = sspr
  hpr2 = sspr2

instance Scalar Double where
  type RealPart Double = Double
  dotu = ddot
  dotc = ddot
  asum = dasum
  nrm2 = dnrm2
  iamax = idamax
  axpy = daxpy
  scal = dscal
  rscal = dscal
  copy = dcopy
  swap = dswap
  gemv = dgemv
  geru = dger
  gerc = dger
  gbmv = dgbmv
  hemv = dsymv
  her = dsyr
  her2 = dsyr2
  hbmv = dsbmv
  hpmv = dspmv
  hpr = dspr
  hpr2 = dspr2

instance Scalar (Complex Float) where
  type RealPart (Complex Float) = Float
  dotu = cdot
  dotc = cdot
  asum = scasum
  nrm2 = scnrm2
  iamax = icamax
  axpy = caxpy
  scal = cscal
  rscal = csscal
  copy = ccopy
  swap = cswap
  gemv = cgemv
  geru = cgeru
  gerc = cgerc
  gbmv = cgbmv
  hemv = chemv
  her = cher
  her2 = cher2
  hbmv = chbmv
  hpmv = chpmv
  hpr = chpr
  hpr2 = chpr2

instance Scalar (Complex Double) where
  type RealPart (Complex Double) = Double
  dotu = zdot
  dotc = zdot
  asum = dzasum
  nrm2 = dznrm2
  iamax = izamax
  axpy = zaxpy
  scal = zscal
  rscal = zdscal
  copy = zcopy
  swap = zswap
  gemv = zgemv
  geru = zgeru
  gerc = zgerc
  gbmv = zgbmv
  hemv = zhemv
  her = zher
  her2 = zher2
  hbmv = zhbmv
  hpmv = zhpmv
  hpr = zhpr
  hpr2 = zhpr2
