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
    -> V k a  -- ^ vector @x@
    -> a  -- ^ scalar @beta@
    -> Mut (V k) (PrimState m) a  -- ^ vector @y@
    -> m ()

  -- | @y <- alpha A x + beta y@
  hemv
    :: PrimMonad m =>
       a
    -> HE n a
    -> V k a
    -> a
    -> Mut (V k) (PrimState m) a
    -> m ()

  -- | @y <- alpha A x + beta y@
  hpmv
    :: PrimMonad m =>
       a
    -> HP n a
    -> V k a
    -> a
    -> Mut (V k) (PrimState m) a
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

{-
instance Scalar Double where
  gbmv alpha a x beta y =
    unsafePrimToPrim $
    unsafeWithGB a $ \trans m n kl ku pa lda ->
    unsafeWithV x $ \_ px incx ->
    withV y $ \_ py incy ->
      [C.block|
        void {
          BLASFUN(dgbmv)
          ( $trans:trans
          , &$(INT m)
          , &$(INT n)
          , &$(INT kl)
          , &$(INT ku)
          , &$(double alpha)
          , $(double* pa)
          , &$(INT lda)
          , $(double* px)
          , &$(INT incx)
          , &$(double beta)
          , $(double* py)
          , &$(INT incy)
          )
        }
      |]

  gemv alpha a x beta y =
    unsafePrimToPrim $
    unsafeWithGE a $ \trans m n pa lda ->
    unsafeWithV x $ \_ px incx ->
    withV y $ \_ py incy ->
      [C.block|
        void {
          BLASFUN(dgemv)
          ( $trans:trans
          , &$(INT m)
          , &$(INT n)
          , $(double alpha)
          , $(double* pa)
          , &$(INT lda)
          , $(double *px)
          , &$(INT incx)
          , $(double beta)
          , $(double* py)
          , &$(INT incy)
          )
        }
      |]

  hbmv alpha a x beta y =
    unsafePrimToPrim $
    unsafeWithHB a $ \uplo nn kk pa lda ->
    unsafeWithV x $ \_ px incx ->
    withV y $ \_ py incy ->
      [C.block|
        void {
          BLASFUN(dsbmv)
          ( $uplo:uplo
          , &$(INT nn)
          , &$(INT kk)
          , $(double alpha)
          , $(double* pa)
          , &$(INT lda)
          , $(double* px)
          , &$(INT incx)
          , $(double beta)
          , $(double* py)
          , &$(INT incy)
          )
        }
      |]

  hemv alpha a x beta y =
    unsafePrimToPrim $
    unsafeWithHE a $ \uplo nn pa lda ->
    unsafeWithV x $ \_ px incx ->
    withV y $ \_ py incy ->
      [C.block|
        void {
          BLASFUN(dsymv)
          ( $uplo:uplo
          , &$(INT nn)
          , $(double alpha)
          , $(double* pa)
          , &$(INT lda)
          , $(double* px)
          , &$(INT incx)
          , $(double beta)
          , $(double* py)
          , &$(INT incy)
          )
        }
      |]

  hpmv alpha a x beta y =
    unsafePrimToPrim $
    unsafeWithHP a $ \uplo nn pa ->
    unsafeWithV x $ \_ px incx ->
    withV y $ \_ py incy ->
      [C.block|
        void {
          BLASFUN(dspmv)
          ( $uplo:uplo
          , &$(INT nn)
          , $(double alpha)
          , $(double* pa)
          , $(double* px)
          , &$(INT incx)
          , $(double beta)
          , $(double* py)
          , &$(INT incy)
          )
        }
      |]

  tbmv a x =
    unsafePrimToPrim $
    unsafeWithTB a $ \uplo trans diag nn kk pa lda ->
    withV x $ \_ px incx ->
      [C.block|
        void {
          BLASFUN(dtbmv)
          ( $uplo:uplo
          , $trans:trans
          , $diag:diag
          , &$(INT nn)
          , &$(INT kk)
          , $(double* pa)
          , &$(INT lda)
          , $(double* px)
          , &$(INT incx)
          )
        }
      |]

  tpmv a x =
    unsafePrimToPrim $
    unsafeWithTP a $ \uplo trans diag nn pa ->
    withV x $ \_ px incx ->
      [C.block|
        void {
          BLASFUN(dtpmv)
          ( $uplo:uplo
          , $trans:trans
          , $diag:diag
          , &$(INT nn)
          , $(double* pa)
          , $(double* px)
          , &$(INT incx)
          )
        }
      |]

  trmv a x =
    unsafePrimToPrim $
    unsafeWithTR a $ \uplo trans diag nn pa lda ->
    withV x $ \_ px incx ->
      [C.block|
        void {
          BLASFUN(dtrmv)
          ( $uplo:uplo
          , $trans:trans
          , $diag:diag
          , &$(INT nn)
          , $(double* pa)
          , &$(INT lda)
          , $(double* px)
          , &$(INT incx)
          )
        }
      |]

  tbsv a x =
    unsafePrimToPrim $
    unsafeWithTB a $ \uplo trans diag nn kk pa lda ->
    withV x $ \_ px incx ->
      [C.block|
        void {
          BLASFUN(dtbsv)
          ( $uplo:uplo
          , $trans:trans
          , $diag:diag
          , &$(INT nn)
          , &$(INT kk)
          , $(double* pa)
          , &$(INT lda)
          , $(double* px)
          , &$(INT incx)
          )
        }
      |]

  tpsv a x =
    unsafePrimToPrim $
    unsafeWithTP a $ \uplo trans diag nn pa ->
    withV x $ \_ px incx ->
      [C.block|
        void {
          BLASFUN(dtpsv)
          ( $uplo:uplo
          , $trans:trans
          , $diag:diag
          , &$(INT nn)
          , $(double* pa)
          , $(double* px)
          , &$(INT incx)
          )
        }
      |]

  trsv a x =
    unsafePrimToPrim $
    unsafeWithTR a $ \uplo trans diag nn pa lda ->
    withV x $ \_ px incx ->
      [C.block|
        void {
          BLASFUN(dtrsv)
          ( $uplo:uplo
          , $trans:trans
          , $diag:diag
          , &$(INT nn)
          , $(double* pa)
          , &$(INT lda)
          , $(double* px)
          , &$(INT incx)
          )
        }
      |]

  geru alpha x y a =
    unsafePrimToPrim $
    unsafeWithV x $ \_ px incx ->
    unsafeWithV y $ \_ py incy ->
    withGE a $ \trans nr nc pa lda ->
      case trans of
        NoTrans ->
          [C.block|
            void {
              BLASFUNC(dger)
              ( &$(INT nr)
              , &$(INT nc)
              , $(double alpha)
              , $(double* px)
              , &$(INT incx)
              , $(double* py)
              , &$(INT incy)
              , $(double* pa)
              , &$(INT lda)
              )
            }
          |]
        _ ->
          [C.block|
            void {
              BLASFUNC(dger)
              ( &$(INT nr)
              , &$(INT nc)
              , $(double alpha)
              , $(double* py)
              , &$(INT incy)
              , $(double* px)
              , &$(INT incx)
              , $(double* pa)
              , &$(INT lda)
              )
            }
          |]

  gerc = geru

  her alpha x a =
    unsafePrimToPrim $
    unsafeWithV x $ \_ px incx ->
    withHE a $ \uplo nn pa lda ->
      [C.block|
        void {
          BLASFUN(dsyr)
          ( $uplo:uplo
          , &$(INT nn)
          , $(double alpha)
          , $(double* px)
          , &$(INT incx)
          , $(double* pa)
          , &$(INT lda)
          )
        }
      |]

  her2 alpha x y a =
    unsafePrimToPrim $
    unsafeWithV x $ \_ px incx ->
    unsafeWithV y $ \_ py incy ->
    withHE a $ \uplo nn pa lda ->
      [C.block|
        void {
          BLASFUN(dsyr2)
          ( $uplo:uplo
          , &$(INT nn)
          , $(double alpha)
          , $(double* px)
          , &$(INT incx)
          , $(double* py)
          , &$(INT incy)
          , $(double* pa)
          , &$(INT lda)
          )
        }
      |]

  hpr alpha x a =
    unsafePrimToPrim $
    unsafeWithV x $ \_ px incx ->
    withHP a $ \uplo nn pa ->
      [C.block|
        void {
          BLASFUN(dhpr)
          ( $uplo:uplo
          , &$(INT nn)
          , $(double alpha)
          , $(double* px)
          , &$(INT incx)
          , $(double* pa)
          )
        }
      |]

  hpr2 alpha x y a =
    unsafePrimToPrim $
    unsafeWithV x $ \_ px incx ->
    unsafeWithV y $ \_ py incy ->
    withHP a $ \uplo nn pa ->
      [C.block|
        void {
          BLASFUN(dhpr2)
          ( $uplo:uplo
          , &$(INT nn)
          , $(double alpha)
          , $(double* px)
          , &$(INT incx)
          , $(double* py)
          , &$(INT incy)
          , $(double* pa)
          )
        }
      |]


instance Scalar (Complex Double) where
  gbmv alpha a x beta y =
    unsafePrimToPrim $
    with alpha $ \palpha ->
    unsafeWithGB a $ \trans m n kl ku pa lda ->
    unsafeWithV x $ \_ px incx ->
    with beta $ \pbeta ->
    withV y $ \_ py incy ->
      [C.block|
        void {
          BLASFUN(zgbmv)
          ( $trans:trans
          , &$(INT m)
          , &$(INT n)
          , &$(INT kl)
          , &$(INT ku)
          , (double*)$(double_complex* palpha)
          , (double*)$(double_complex* pa)
          , &$(INT lda)
          , (double*)$(double_complex* px)
          , &$(INT incx)
          , (double*)$(double_complex* pbeta)
          , (double*)$(double_complex* py)
          , &$(INT incy)
          )
        }
      |]

  gemv alpha a x beta y =
    unsafePrimToPrim $
    with alpha $ \palpha ->
    unsafeWithGE a $ \trans m n pa lda ->
    unsafeWithV x $ \_ px incx ->
    with beta $ \pbeta ->
    withV y $ \_ py incy ->
      [C.block|
        void {
          BLASFUN(zgemv)
          ( $trans:trans
          , &$(INT m)
          , &$(INT n)
          , (double*)$(double_complex* palpha)
          , (double*)$(double_complex* pa)
          , &$(INT lda)
          , (double*)$(double_complex* px)
          , &$(INT incx)
          , (double*)$(double_complex* pbeta)
          , (double*)$(double_complex* py)
          , &$(INT incy)
          )
        }
      |]

  hbmv alpha a x beta y =
    unsafePrimToPrim $
    with alpha $ \palpha ->
    unsafeWithHB a $ \uplo nn kk pa lda ->
    unsafeWithV x $ \_ px incx ->
    with beta $ \pbeta ->
    withV y $ \_ py incy ->
      [C.block|
        void {
          BLASFUN(zhbmv)
          ( $uplo:uplo
          , &$(INT nn)
          , &$(INT kk)
          , (double*)$(double_complex* palpha)
          , (double*)$(double_complex* pa)
          , &$(INT lda)
          , (double*)$(double_complex* px)
          , &$(INT incx)
          , (double*)$(double_complex* pbeta)
          , (double*)$(double_complex* py)
          , &$(INT incy)
          )
        }
      |]

  hemv alpha a x beta y =
    unsafePrimToPrim $
    with alpha $ \palpha ->
    unsafeWithHE a $ \uplo nn pa lda ->
    unsafeWithV x $ \_ px incx ->
    with beta $ \pbeta ->
    withV y $ \_ py incy ->
      [C.block|
        void {
          BLASFUN(zhemv)
          ( $uplo:uplo
          , &$(INT nn)
          , (double*)$(double_complex* palpha)
          , (double*)$(double_complex* pa)
          , &$(INT lda)
          , (double*)$(double_complex* px)
          , &$(INT incx)
          , (double*)$(double_complex* pbeta)
          , (double*)$(double_complex* py)
          , &$(INT incy)
          )
        }
      |]

  hpmv alpha a x beta y =
    unsafePrimToPrim $
    with alpha $ \palpha ->
    unsafeWithHP a $ \uplo nn pa ->
    unsafeWithV x $ \_ px incx ->
    with beta $ \pbeta ->
    withV y $ \_ py incy ->
      [C.block|
        void {
          BLASFUN(zhpmv)
          ( $uplo:uplo
          , &$(INT nn)
          , (double*)$(double_complex* palpha)
          , (double*)$(double_complex* pa)
          , (double*)$(double_complex* px)
          , &$(INT incx)
          , (double*)$(double_complex* pbeta)
          , (double*)$(double_complex* py)
          , &$(INT incy)
          )
        }
      |]

  tbmv a x =
    unsafePrimToPrim $
    unsafeWithTB a $ \uplo trans diag nn kk pa lda ->
    withV x $ \_ px incx ->
      [C.block|
        void {
          BLASFUN(ztbmv)
          ( $uplo:uplo
          , $trans:trans
          , $diag:diag
          , &$(INT nn)
          , &$(INT kk)
          , (double*)$(double_complex* pa)
          , &$(INT lda)
          , (double*)$(double_complex* px)
          , &$(INT incx)
          )
        }
      |]

  tpmv a x =
    unsafePrimToPrim $
    unsafeWithTP a $ \uplo trans diag nn pa ->
    withV x $ \_ px incx ->
      [C.block|
        void {
          BLASFUN(ztpmv)
          ( $uplo:uplo
          , $trans:trans
          , $diag:diag
          , &$(INT nn)
          , (double*)$(double_complex* pa)
          , (double*)$(double_complex* px)
          , &$(INT incx)
          )
        }
      |]

  trmv a x =
    unsafePrimToPrim $
    unsafeWithTR a $ \uplo trans diag nn pa lda ->
    withV x $ \_ px incx ->
      [C.block|
        void {
          BLASFUN(ztrmv)
          ( $uplo:uplo
          , $trans:trans
          , $diag:diag
          , &$(INT nn)
          , (double*)$(double_complex* pa)
          , &$(INT lda)
          , (double*)$(double_complex* px)
          , &$(INT incx)
          )
        }
      |]

  tbsv a x =
    unsafePrimToPrim $
    unsafeWithTB a $ \uplo trans diag nn kk pa lda ->
    withV x $ \_ px incx ->
      [C.block|
        void {
          BLASFUN(ztbsv)
          ( $uplo:uplo
          , $trans:trans
          , $diag:diag
          , &$(INT nn)
          , &$(INT kk)
          , (double*)$(double_complex* pa)
          , &$(INT lda)
          , (double*)$(double_complex* px)
          , &$(INT incx)
          )
        }
      |]

  tpsv a x =
    unsafePrimToPrim $
    unsafeWithTP a $ \uplo trans diag nn pa ->
    withV x $ \_ px incx ->
      [C.block|
        void {
          BLASFUN(ztpsv)
          ( $uplo:uplo
          , $trans:trans
          , $diag:diag
          , &$(INT nn)
          , (double*)$(double_complex* pa)
          , (double*)$(double_complex* px)
          , &$(INT incx)
          )
        }
      |]

  trsv a x =
    unsafePrimToPrim $
    unsafeWithTR a $ \uplo trans diag nn pa lda ->
    withV x $ \_ px incx ->
      [C.block|
        void {
          BLASFUN(ztrsv)
          ( $uplo:uplo
          , $trans:trans
          , $diag:diag
          , &$(INT nn)
          , (double*)$(double_complex* pa)
          , &$(INT lda)
          , (double*)$(double_complex* px)
          , &$(INT incx)
          )
        }
      |]

  geru alpha x y a =
    unsafePrimToPrim $
    unsafeWithV x $ \_ px incx ->
    withGE a $ \trans nr nc pa lda ->
      case trans of
        NoTrans ->
          with alpha $ \palpha ->
          unsafeWithV y $ \_ py incy ->
            [C.block|
              void {
                BLASFUNC(zgeru)
                ( &$(INT nr)
                , &$(INT nc)
                , (double*)$(double_complex* palpha)
                , (double*)$(double_complex* px)
                , &$(INT incx)
                , (double*)$(double_complex* py)
                , &$(INT incy)
                , (double*)$(double_complex* pa)
                , &$(INT lda)
                )
              }
            |]
        Trans ->
          with alpha $ \palpha ->
          unsafeWithV y $ \_ py incy ->
            [C.block|
              void {
                BLASFUNC(zgeru)
                ( &$(INT nr)
                , &$(INT nc)
                , (double*)$(double_complex* palpha)
                , (double*)$(double_complex* py)
                , &$(INT incy)
                , (double*)$(double_complex* px)
                , &$(INT incx)
                , (double*)$(double_complex* pa)
                , &$(INT lda)
                )
              }
            |]
        ConjTrans ->
          with (conjugate alpha) $ \palpha ->
          unsafeWithV (V.map conjugate y) $ \_ py incy ->
            [C.block|
              void {
                BLASFUNC(zgerc)
                ( &$(INT nr)
                , &$(INT nc)
                , (double*)$(double_complex* palpha)
                , (double*)$(double_complex* py)
                , &$(INT incy)
                , (double*)$(double_complex* px)
                , &$(INT incx)
                , (double*)$(double_complex* pa)
                , &$(INT lda)
                )
              }
            |]

  gerc alpha x y a =
    unsafePrimToPrim $
    unsafeWithV x $ \_ px incx ->
    withGE a $ \trans nr nc pa lda ->
      case trans of
        NoTrans ->
          with alpha $ \palpha ->
          unsafeWithV y $ \_ py incy ->
            [C.block|
              void {
                BLASFUNC(zgerc)
                ( &$(INT nr)
                , &$(INT nc)
                , (double*)$(double_complex* palpha)
                , (double*)$(double_complex* px)
                , &$(INT incx)
                , (double*)$(double_complex* py)
                , &$(INT incy)
                , (double*)$(double_complex* pa)
                , &$(INT lda)
                )
              }
            |]
        Trans ->
          with alpha $ \palpha ->
          unsafeWithV (V.map conjugate y) $ \_ py incy ->
            [C.block|
              void {
                BLASFUNC(zgeru)
                ( &$(INT nr)
                , &$(INT nc)
                , (double*)$(double_complex* palpha)
                , (double*)$(double_complex* py)
                , &$(INT incy)
                , (double*)$(double_complex* px)
                , &$(INT incx)
                , (double*)$(double_complex* pa)
                , &$(INT lda)
                )
              }
            |]
        ConjTrans ->
          with (conjugate alpha) $ \palpha ->
          unsafeWithV y $ \_ py incy ->
            [C.block|
              void {
                BLASFUNC(zgerc)
                ( &$(INT nr)
                , &$(INT nc)
                , (double*)$(double_complex* palpha)
                , (double*)$(double_complex* py)
                , &$(INT incy)
                , (double*)$(double_complex* px)
                , &$(INT incx)
                , (double*)$(double_complex* pa)
                , &$(INT lda)
                )
              }
            |]

  her alpha x a =
    unsafePrimToPrim $
    unsafeWithV x $ \_ px incx ->
    withHE a $ \uplo nn pa lda ->
      [C.block|
        void {
          BLASFUN(zher)
          ( $uplo:uplo
          , &$(INT nn)
          , $(double alpha)
          , (double*)$(double_complex* px)
          , &$(INT incx)
          , (double*)$(double_complex* pa)
          , &$(INT lda)
          )
        }
      |]

  her2 alpha x y a =
    unsafePrimToPrim $
    with alpha $ \palpha ->
    unsafeWithV x $ \_ px incx ->
    unsafeWithV y $ \_ py incy ->
    withHE a $ \uplo nn pa lda ->
      [C.block|
        void {
          BLASFUN(zher2)
          ( $uplo:uplo
          , $(const INT nn)
          , $(double_complex* palpha)
          , $(double_complex* px)
          , $(const INT incx)
          , $(double_complex* py)
          , $(const INT incy)
          , $(double_complex* pa)
          , $(const INT lda)
          )
        }
      |]

  hpr alpha x a =
    unsafePrimToPrim $
    unsafeWithV x $ \_ px incx ->
    withHP a $ \uplo nn pa ->
      [C.block|
        void {
          cblas_zhpr
          ( $uplo:uplo
          , $(const INT nn)
          , $(const double alpha)
          , $(const double_complex* px)
          , $(const INT incx)
          , $(double_complex* pa)
          )
        }
      |]

  hpr2 alpha x y a =
    unsafePrimToPrim $
    with alpha $ \palpha ->
    unsafeWithV x $ \_ px incx ->
    unsafeWithV y $ \_ py incy ->
    withHP a $ \uplo nn pa ->
      [C.block|
        void {
          cblas_zhpr2
          ( $uplo:uplo
          , $(const INT nn)
          , $(const double_complex* palpha)
          , $(const double_complex* px)
          , $(const INT incx)
          , $(const double_complex* py)
          , $(const INT incy)
          , $(double_complex* pa)
          )
        }
      |]
-}
