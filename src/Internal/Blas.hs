{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Internal.Blas where

import Control.Monad.Primitive
import Data.Complex
import Data.Monoid ((<>))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Utils (with)
import Foreign.Storable (Storable, peek)
import Language.C.Inline.Context (baseCtx)
import Prelude hiding (length)
import System.IO.Unsafe (unsafePerformIO)

import qualified Data.Vector.Generic as V
import qualified Language.C.Inline as C

import Internal.Int
import Internal.Matrix
import Internal.Mut
import Internal.Vector
import Language.C.Inline.Context.Blas

C.context (baseCtx <> blasCtx)
C.include "<cblas.h>"
C.include "<f77blas.h>"


class Storable a => Scalar a where
  type RealPart a

  -- | Unconjugated inner (dot) product, @x^T x@.
  dotu :: V n a -> V n a -> a

  -- | Conjugated inner (dot) product, @x^H x@.
  dotc :: V n a -> V n a -> a

  -- | The sum of the real modulus of each element of the vector,
  -- @sum . map magnitude@.
  asum :: V n a -> RealPart a

  -- | The Euclidean norm of the vector,
  -- @sqrt . sum . map (\x -> realPart (conjugate x * x))@
  nrm2 :: V n a -> RealPart a

  -- | The index of the element of the vector with the largest real
  -- modulus.
  iamax :: V n a -> I
  amax :: V n a -> RealPart a

  -- | The index of the element of the vector with the smallest real
  -- modulus.
  iamin :: V n a -> I
  amin :: V n a -> RealPart a

  max :: V n a -> RealPart a
  min :: V n a -> RealPart a

  -- | @y <- a x + y@
  axpy
    :: PrimMonad m =>
       a  -- ^ scalar @a@
    -> V n a  -- ^ vector @x@
    -> Mut (V n) (PrimState m) a  -- ^ vector @y@
    -> m ()

  -- | @y <- a conj(x) + y@
  axpyc
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
instance Scalar Double where
  type RealPart Double = Double

  dotu x y =
    unsafePerformIO $
    unsafeWithV x $ \n ptrx incx ->
    unsafeWithV y $ \_ ptry incy ->
      [C.exp|
        double {
          BLASFUNC(ddot)
          ( &$(blasint n)
          , $(double* ptrx)
          , &$(blasint incx)
          , $(double* ptry)
          , &$(blasint incy)
          )
        }
      |]

  dotc = dotu

  asum x =
    unsafePerformIO $
    unsafeWithV x $ \n ptr inc ->
      [C.exp|
        double {
          BLASFUNC(dasum)
          ( &$(blasint n)
          , $(double* ptr)
          , &$(blasint inc)
          )
        }
      |]

  iamax x =
    unsafePerformIO $
    unsafeWithV x $ \n ptr inc ->
      [C.exp|
        blasint {
          BLASFUNC(idamax)
          ( &$(blasint n)
          , $(double* ptr)
          , &$(blasint inc)
          )
        }
      |]

  amax x =
    unsafePerformIO $
    unsafeWithV x $ \n ptr inc ->
      [C.exp|
        double {
          BLASFUNC(damax)
          ( &$(blasint n)
          , $(double* ptr)
          , &$(blasint inc)
          )
        }
      |]

  iamin x =
    unsafePerformIO $
    unsafeWithV x $ \n ptr inc ->
      [C.exp|
        blasint {
          BLASFUNC(idamin)
          ( &$(blasint n)
          , $(double* ptr)
          , &$(blasint inc)
          )
        }
      |]

  amin x =
    unsafePerformIO $
    unsafeWithV x $ \n ptr inc ->
      [C.exp|
        double {
          BLASFUNC(damin)
          ( &$(blasint n)
          , $(double* ptr)
          , &$(blasint inc)
          )
        }
      |]

  max x =
    unsafePerformIO $
    unsafeWithV x $ \n ptr inc ->
      [C.exp|
        double {
          BLASFUNC(dmax)
          ( &$(blasint n)
          , $(double* ptr)
          , &$(blasint inc)
          )
        }
      |]

  min x =
    unsafePerformIO $
    unsafeWithV x $ \n ptr inc ->
      [C.exp|
        double {
          BLASFUNC(dmin)
          ( &$(blasint n)
          , $(double* ptr)
          , &$(blasint inc)
          )
        }
      |]

  nrm2 x =
    unsafePerformIO $
    unsafeWithV x $ \n ptr inc ->
      [C.exp|
        double {
          BLASFUNC(dnrm2)
          ( &$(blasint n)
          , $(double* ptr)
          , &$(blasint inc)
          )
        }
      |]

  axpy a x y =
    unsafePrimToPrim $
    unsafeWithV x $ \n ptrx incx ->
    withV y $ \_ ptry incy ->
      [C.block|
        void {
          BLASFUNC(daxpy)
          ( &$(blasint n)
          , &$(double a)
          , $(double* ptrx)
          , &$(blasint incx)
          , $(double* ptry)
          , &$(blasint incy)
          );
        }
      |]

  axpyc = axpy

  scal a x =
    unsafePrimToPrim $
    withV x $ \n ptr inc ->
      [C.exp|
        void {
          BLASFUNC(dscal)
          ( &$(blasint n)
          , &$(double a)
          , $(double* ptr)
          , &$(blasint inc)
          )
        }
      |]

  rscal = scal

  copy x y =
    unsafePrimToPrim $
    unsafeWithV x $ \n ptrx incx ->
    withV y $ \_ ptry incy ->
      [C.exp|
        void {
          BLASFUNC(dcopy)
          ( &$(blasint n)
          , $(double* ptrx)
          , &$(blasint incx)
          , $(double* ptry)
          , &$(blasint incy)
          )
        }
      |]

  swap x y =
    unsafePrimToPrim $
    withV x $ \n ptrx incx ->
    withV y $ \_ ptry incy ->
      [C.exp|
        void {
          BLASFUNC(dswap)
          ( &$(blasint n)
          , $(double* ptrx)
          , &$(blasint incx)
          , $(double* ptry)
          , &$(blasint incy)
          )
        }
      |]

  gbmv alpha a x beta y =
    unsafePrimToPrim $
    unsafeWithGB a $ \trans m n kl ku pa lda ->
    unsafeWithV x $ \_ px incx ->
    withV y $ \_ py incy ->
      [C.block|
        void {
          BLASFUN(dgbmv)
          ( $trans:trans
          , &$(blasint m)
          , &$(blasint n)
          , &$(blasint kl)
          , &$(blasint ku)
          , &$(double alpha)
          , $(double* pa)
          , &$(blasint lda)
          , $(double* px)
          , &$(blasint incx)
          , &$(double beta)
          , $(double* py)
          , &$(blasint incy)
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
          , &$(blasint m)
          , &$(blasint n)
          , $(double alpha)
          , $(double* pa)
          , &$(blasint lda)
          , $(double *px)
          , &$(blasint incx)
          , $(double beta)
          , $(double* py)
          , &$(blasint incy)
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
          , &$(blasint nn)
          , &$(blasint kk)
          , $(double alpha)
          , $(double* pa)
          , &$(blasint lda)
          , $(double* px)
          , &$(blasint incx)
          , $(double beta)
          , $(double* py)
          , &$(blasint incy)
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
          , &$(blasint nn)
          , $(double alpha)
          , $(double* pa)
          , &$(blasint lda)
          , $(double* px)
          , &$(blasint incx)
          , $(double beta)
          , $(double* py)
          , &$(blasint incy)
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
          , &$(blasint nn)
          , $(double alpha)
          , $(double* pa)
          , $(double* px)
          , &$(blasint incx)
          , $(double beta)
          , $(double* py)
          , &$(blasint incy)
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
          , &$(blasint nn)
          , &$(blasint kk)
          , $(double* pa)
          , &$(blasint lda)
          , $(double* px)
          , &$(blasint incx)
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
          , &$(blasint nn)
          , $(double* pa)
          , $(double* px)
          , &$(blasint incx)
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
          , &$(blasint nn)
          , $(double* pa)
          , &$(blasint lda)
          , $(double* px)
          , &$(blasint incx)
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
          , &$(blasint nn)
          , &$(blasint kk)
          , $(double* pa)
          , &$(blasint lda)
          , $(double* px)
          , &$(blasint incx)
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
          , &$(blasint nn)
          , $(double* pa)
          , $(double* px)
          , &$(blasint incx)
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
          , &$(blasint nn)
          , $(double* pa)
          , &$(blasint lda)
          , $(double* px)
          , &$(blasint incx)
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
              ( &$(blasint nr)
              , &$(blasint nc)
              , $(double alpha)
              , $(double* px)
              , &$(blasint incx)
              , $(double* py)
              , &$(blasint incy)
              , $(double* pa)
              , &$(blasint lda)
              )
            }
          |]
        _ ->
          [C.block|
            void {
              BLASFUNC(dger)
              ( &$(blasint nr)
              , &$(blasint nc)
              , $(double alpha)
              , $(double* py)
              , &$(blasint incy)
              , $(double* px)
              , &$(blasint incx)
              , $(double* pa)
              , &$(blasint lda)
              )
            }
          |]

  gerc = geru

instance Scalar (Complex Double) where
  type RealPart (Complex Double) = Double

  dotu x y =
    unsafePerformIO $
    unsafeWithV x $ \n ptrx incx ->
    unsafeWithV y $ \_ ptry incy ->
    alloca $ \z -> do
      [C.block|
        void {
          #ifdef RETURN_BY_STACK
          BLASFUNC(zdotu)
          ( $(openblas_complex_double* z)
          , &$(blasint n)
          , (double*)$(openblas_complex_double* ptrx)
          , &$(blasint incx)
          , (double*)$(openblas_complex_double* ptry)
          , &$(blasint incy)
          )
          #else
          *$(openblas_complex_double* z) =
          BLASFUNC(zdotu)
          ( &$(blasint n)
          , (double*)$(openblas_complex_double* ptrx)
          , &$(blasint incx)
          , (double*)$(openblas_complex_double* ptry)
          , &$(blasint incy)
          );
          #endif
        }
      |]
      peek z

  dotc x y =
    unsafePerformIO $
    unsafeWithV x $ \n ptrx incx ->
    unsafeWithV y $ \_ ptry incy ->
    alloca $ \z -> do
      [C.block|
        void {
          #ifdef RETURN_BY_STACK
          BLASFUNC(zdotc)
          ( $(openblas_complex_double* z)
          , &$(blasint n)
          , (double*)$(openblas_complex_double* ptrx)
          , &$(blasint incx)
          , (double*)$(openblas_complex_double* ptry)
          , &$(blasint incy)
          )
          #else
          *$(openblas_complex_double* z) =
          BLASFUNC(zdotc)
          ( &$(blasint n)
          , (double*)$(openblas_complex_double* ptrx)
          , &$(blasint incx)
          , (double*)$(openblas_complex_double* ptry)
          , &$(blasint incy)
          );
          #endif
        }
      |]
      peek z

  asum x =
    unsafePerformIO $
    unsafeWithV x $ \n ptr inc ->
      [C.exp|
        double {
          BLASFUNC(dzasum)
          ( &$(blasint n)
          , (double*)$(openblas_complex_double* ptr)
          , &$(blasint inc)
          )
        }
      |]

  iamax x =
    unsafePerformIO $
    unsafeWithV x $ \n ptr inc ->
      [C.exp|
        blasint {
          BLASFUNC(izamax)
          ( &$(blasint n)
          , (double*)$(openblas_complex_double* ptr)
          , &$(blasint inc)
          )
        }
      |]

  amax x =
    unsafePerformIO $
    unsafeWithV x $ \n ptr inc ->
      [C.exp|
        double {
          BLASFUNC(dzamax)
          ( &$(blasint n)
          , (double*)$(openblas_complex_double* ptr)
          , &$(blasint inc)
          )
        }
      |]

  iamin x =
    unsafePerformIO $
    unsafeWithV x $ \n ptr inc ->
      [C.exp|
        blasint {
          BLASFUNC(izamin)
          ( &$(blasint n)
          , (double*)$(openblas_complex_double* ptr)
          , &$(blasint inc)
          )
        }
      |]

  amin x =
    unsafePerformIO $
    unsafeWithV x $ \n ptr inc ->
      [C.exp|
        double {
          BLASFUNC(dzamin)
          ( &$(blasint n)
          , (double*)$(openblas_complex_double* ptr)
          , &$(blasint inc)
          )
        }
      |]

  max x =
    unsafePerformIO $
    unsafeWithV x $ \n ptr inc ->
      [C.exp|
        double {
          BLASFUNC(dzmax)
          ( &$(blasint n)
          , (double*)$(openblas_complex_double* ptr)
          , &$(blasint inc)
          )
        }
      |]

  min x =
    unsafePerformIO $
    unsafeWithV x $ \n ptr inc ->
      [C.exp|
        double {
          BLASFUNC(dzmin)
          ( &$(blasint n)
          , (double*)$(openblas_complex_double* ptr)
          , &$(blasint inc)
          )
        }
      |]

  nrm2 x =
    unsafePerformIO $
    unsafeWithV x $ \n ptr inc ->
      [C.exp|
        double {
          BLASFUNC(dznrm2)
          ( &$(blasint n)
          , (double*)$(openblas_complex_double* ptr)
          , &$(blasint inc)
          )
        }
      |]

  axpy a x y =
    unsafePrimToPrim $
    unsafeWithV x $ \n ptrx incx ->
    withV y $ \_ ptry incy ->
    with a $ \pa ->
      [C.block|
        void {
          BLASFUNC(zaxpy)
          ( &$(blasint n)
          , (double*)$(openblas_complex_double* pa)
          , (double*)$(openblas_complex_double* ptrx)
          , &$(blasint incx)
          , (double*)$(openblas_complex_double* ptry)
          , &$(blasint incy)
          );
        }
      |]

  axpyc a x y =
    unsafePrimToPrim $
    unsafeWithV x $ \n ptrx incx ->
    withV y $ \_ ptry incy ->
    with a $ \pa ->
      [C.block|
        void {
          BLASFUNC(zaxpyc)
          ( &$(blasint n)
          , (double*)$(openblas_complex_double* pa)
          , (double*)$(openblas_complex_double* ptrx)
          , &$(blasint incx)
          , (double*)$(openblas_complex_double* ptry)
          , &$(blasint incy)
          );
        }
      |]

  scal a x =
    unsafePrimToPrim $
    withV x $ \n ptr inc ->
    with a $ \pa ->
      [C.block|
        void {
          BLASFUNC(zscal)
          ( &$(blasint n)
          , (double*)$(openblas_complex_double* pa)
          , (double*)$(openblas_complex_double* ptr)
          , &$(blasint inc)
          );
        }
      |]

  rscal a x =
    unsafePrimToPrim $
    withV x $ \n ptr inc ->
    with a $ \pa ->
      [C.block|
        void {
          BLASFUNC(zdscal)
          ( &$(blasint n)
          , $(double* pa)
          , (double*)$(openblas_complex_double* ptr)
          , &$(blasint inc)
          );
        }
      |]

  copy x y =
    unsafePrimToPrim $
    unsafeWithV x $ \n ptrx incx ->
    withV y $ \_ ptry incy ->
      [C.exp|
        void {
          BLASFUNC(zcopy)
          ( &$(blasint n)
          , (double*)$(openblas_complex_double* ptrx)
          , &$(blasint incx)
          , (double*)$(openblas_complex_double* ptry)
          , &$(blasint incy)
          )
        }
      |]

  swap x y =
    unsafePrimToPrim $
    withV x $ \n ptrx incx ->
    withV y $ \_ ptry incy ->
      [C.exp|
        void {
          cblas_zswap
          ( &$(blasint n)
          , (double*)$(openblas_complex_double* ptrx)
          , &$(blasint incx)
          , (double*)$(openblas_complex_double* ptry)
          , &$(blasint incy)
          )
        }
      |]

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
          , &$(blasint m)
          , &$(blasint n)
          , &$(blasint kl)
          , &$(blasint ku)
          , (double*)$(openblas_complex_double* palpha)
          , (double*)$(openblas_complex_double* pa)
          , &$(blasint lda)
          , (double*)$(openblas_complex_double* px)
          , &$(blasint incx)
          , (double*)$(openblas_complex_double* pbeta)
          , (double*)$(openblas_complex_double* py)
          , &$(blasint incy)
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
          , &$(blasint m)
          , &$(blasint n)
          , (double*)$(openblas_complex_double* palpha)
          , (double*)$(openblas_complex_double* pa)
          , &$(blasint lda)
          , (double*)$(openblas_complex_double* px)
          , &$(blasint incx)
          , (double*)$(openblas_complex_double* pbeta)
          , (double*)$(openblas_complex_double* py)
          , &$(blasint incy)
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
          , &$(blasint nn)
          , &$(blasint kk)
          , (double*)$(openblas_complex_double* palpha)
          , (double*)$(openblas_complex_double* pa)
          , &$(blasint lda)
          , (double*)$(openblas_complex_double* px)
          , &$(blasint incx)
          , (double*)$(openblas_complex_double* pbeta)
          , (double*)$(openblas_complex_double* py)
          , &$(blasint incy)
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
          , &$(blasint nn)
          , (double*)$(openblas_complex_double* palpha)
          , (double*)$(openblas_complex_double* pa)
          , &$(blasint lda)
          , (double*)$(openblas_complex_double* px)
          , &$(blasint incx)
          , (double*)$(openblas_complex_double* pbeta)
          , (double*)$(openblas_complex_double* py)
          , &$(blasint incy)
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
          , &$(blasint nn)
          , (double*)$(openblas_complex_double* palpha)
          , (double*)$(openblas_complex_double* pa)
          , (double*)$(openblas_complex_double* px)
          , &$(blasint incx)
          , (double*)$(openblas_complex_double* pbeta)
          , (double*)$(openblas_complex_double* py)
          , &$(blasint incy)
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
          , &$(blasint nn)
          , &$(blasint kk)
          , (double*)$(openblas_complex_double* pa)
          , &$(blasint lda)
          , (double*)$(openblas_complex_double* px)
          , &$(blasint incx)
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
          , &$(blasint nn)
          , (double*)$(openblas_complex_double* pa)
          , (double*)$(openblas_complex_double* px)
          , &$(blasint incx)
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
          , &$(blasint nn)
          , (double*)$(openblas_complex_double* pa)
          , &$(blasint lda)
          , (double*)$(openblas_complex_double* px)
          , &$(blasint incx)
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
          , &$(blasint nn)
          , &$(blasint kk)
          , (double*)$(openblas_complex_double* pa)
          , &$(blasint lda)
          , (double*)$(openblas_complex_double* px)
          , &$(blasint incx)
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
          , &$(blasint nn)
          , (double*)$(openblas_complex_double* pa)
          , (double*)$(openblas_complex_double* px)
          , &$(blasint incx)
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
          , &$(blasint nn)
          , (double*)$(openblas_complex_double* pa)
          , &$(blasint lda)
          , (double*)$(openblas_complex_double* px)
          , &$(blasint incx)
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
                ( &$(blasint nr)
                , &$(blasint nc)
                , (double*)$(openblas_complex_double* palpha)
                , (double*)$(openblas_complex_double* px)
                , &$(blasint incx)
                , (double*)$(openblas_complex_double* py)
                , &$(blasint incy)
                , (double*)$(openblas_complex_double* pa)
                , &$(blasint lda)
                )
              }
            |]
        Trans ->
          with alpha $ \palpha ->
          unsafeWithV y $ \_ py incy ->
            [C.block|
              void {
                BLASFUNC(zgeru)
                ( &$(blasint nr)
                , &$(blasint nc)
                , (double*)$(openblas_complex_double* palpha)
                , (double*)$(openblas_complex_double* py)
                , &$(blasint incy)
                , (double*)$(openblas_complex_double* px)
                , &$(blasint incx)
                , (double*)$(openblas_complex_double* pa)
                , &$(blasint lda)
                )
              }
            |]
        ConjTrans ->
          with (conjugate alpha) $ \palpha ->
          unsafeWithV (V.map conjugate y) $ \_ py incy ->
            [C.block|
              void {
                BLASFUNC(zgerc)
                ( &$(blasint nr)
                , &$(blasint nc)
                , (double*)$(openblas_complex_double* palpha)
                , (double*)$(openblas_complex_double* py)
                , &$(blasint incy)
                , (double*)$(openblas_complex_double* px)
                , &$(blasint incx)
                , (double*)$(openblas_complex_double* pa)
                , &$(blasint lda)
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
                ( &$(blasint nr)
                , &$(blasint nc)
                , (double*)$(openblas_complex_double* palpha)
                , (double*)$(openblas_complex_double* px)
                , &$(blasint incx)
                , (double*)$(openblas_complex_double* py)
                , &$(blasint incy)
                , (double*)$(openblas_complex_double* pa)
                , &$(blasint lda)
                )
              }
            |]
        Trans ->
          with alpha $ \palpha ->
          unsafeWithV (V.map conjugate y) $ \_ py incy ->
            [C.block|
              void {
                BLASFUNC(zgeru)
                ( &$(blasint nr)
                , &$(blasint nc)
                , (double*)$(openblas_complex_double* palpha)
                , (double*)$(openblas_complex_double* py)
                , &$(blasint incy)
                , (double*)$(openblas_complex_double* px)
                , &$(blasint incx)
                , (double*)$(openblas_complex_double* pa)
                , &$(blasint lda)
                )
              }
            |]
        ConjTrans ->
          with (conjugate alpha) $ \palpha ->
          unsafeWithV y $ \_ py incy ->
            [C.block|
              void {
                BLASFUNC(zgerc)
                ( &$(blasint nr)
                , &$(blasint nc)
                , (double*)$(openblas_complex_double* palpha)
                , (double*)$(openblas_complex_double* py)
                , &$(blasint incy)
                , (double*)$(openblas_complex_double* px)
                , &$(blasint incx)
                , (double*)$(openblas_complex_double* pa)
                , &$(blasint lda)
                )
              }
            |]
