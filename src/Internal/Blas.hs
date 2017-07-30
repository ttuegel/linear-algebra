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

import qualified Language.C.Inline as C

import Internal.Int
import Internal.Matrix
import Internal.Mut
import Internal.Vector
import Language.C.Inline.Context.Blas

C.context (baseCtx <> blasCtx)
C.include "<cblas.h>"


class Storable a => Scalar a where
  dotu :: V n a -> V n a -> a
  dotc :: V n a -> V n a -> a
  asum :: V n a -> Double
  nrm2 :: V n a -> Double
  amax :: V n a -> I
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

  -- | @y <- x@
  copy
    :: PrimMonad m =>
       V n a  -- ^ vector @x@
    -> Mut (V n) (PrimState m) a  -- ^ vector @y@
    -> m ()
  swap
    :: PrimMonad m =>
       Mut (V n) (PrimState m) a  -- ^ vector @x@
    -> Mut (V n) (PrimState m) a  -- ^ vector @y@
    -> m ()

instance Scalar Double where
  dotu x y =
    unsafePerformIO $
    unsafeWithV x $ \n ptrx incx ->
    unsafeWithV y $ \_ ptry incy ->
      [C.exp|
        double {
          cblas_ddot
          ( $(blasint n)
          , $(double* ptrx)
          , $(blasint incx)
          , $(double* ptry)
          , $(blasint incy)
          )
        }
      |]

  dotc x y =
    unsafePerformIO $
    unsafeWithV x $ \n ptrx incx ->
    unsafeWithV y $ \_ ptry incy ->
      [C.exp|
        double {
          cblas_ddot
          ( $(blasint n)
          , $(double* ptrx)
          , $(blasint incx)
          , $(double* ptry)
          , $(blasint incy)
          )
        }
      |]

  asum x =
    unsafePerformIO $
    unsafeWithV x $ \n ptr inc ->
      [C.exp|
        double {
          cblas_dasum
          ( $(blasint n)
          , $(double* ptr)
          , $(blasint inc)
          )
        }
      |]

  nrm2 x =
    unsafePerformIO $
    unsafeWithV x $ \n ptr inc ->
      [C.exp|
        double {
          cblas_dnrm2
          ( $(blasint n)
          , $(double* ptr)
          , $(blasint inc)
          )
        }
      |]

  amax x =
    unsafePerformIO $
    unsafeWithV x $ \n ptr inc ->
      [C.exp|
        blasint {
          cblas_idamax
          ( $(blasint n)
          , $(double* ptr)
          , $(blasint inc)
          )
        }
      |]

  axpy a x y =
    unsafePrimToPrim $
    unsafeWithV x $ \n ptrx incx ->
    withV y $ \_ ptry incy ->
      [C.block|
        void {
          cblas_daxpy
          ( $(blasint n)
          , $(double a)
          , $(double* ptrx)
          , $(blasint incx)
          , $(double* ptry)
          , $(blasint incy)
          );
        }
      |]

  scal a x =
    unsafePrimToPrim $
    withV x $ \n ptr inc ->
      [C.exp|
        void {
          cblas_dscal
          ( $(blasint n)
          , $(double a)
          , $(double* ptr)
          , $(blasint inc)
          )
        }
      |]

  copy x y =
    unsafePrimToPrim $
    unsafeWithV x $ \n ptrx incx ->
    withV y $ \_ ptry incy ->
      [C.exp|
        void {
          cblas_dcopy
          ( $(blasint n)
          , $(double* ptrx)
          , $(blasint incx)
          , $(double* ptry)
          , $(blasint incy)
          )
        }
      |]

  swap x y =
    unsafePrimToPrim $
    withV x $ \n ptrx incx ->
    withV y $ \_ ptry incy ->
      [C.exp|
        void {
          cblas_dswap
          ( $(blasint n)
          , $(double* ptrx)
          , $(blasint incx)
          , $(double* ptry)
          , $(blasint incy)
          )
        }
      |]

instance Scalar (Complex Double) where
  dotu x y =
    unsafePerformIO $
    unsafeWithV x $ \n ptrx incx ->
    unsafeWithV y $ \_ ptry incy ->
    alloca $ \z -> do
      [C.block|
        void {
          *$(openblas_complex_double* z) =
              cblas_zdotu
              ( $(blasint n)
              , (double*)$(openblas_complex_double* ptrx)
              , $(blasint incx)
              , (double*)$(openblas_complex_double* ptry)
              , $(blasint incy)
              );
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
          *$(openblas_complex_double* z) =
              cblas_zdotc
              ( $(blasint n)
              , (double*)$(openblas_complex_double* ptrx)
              , $(blasint incx)
              , (double*)$(openblas_complex_double* ptry)
              , $(blasint incy)
              );
        }
      |]
      peek z

  asum x =
    unsafePerformIO $
    unsafeWithV x $ \n ptr inc ->
      [C.exp|
        double {
          cblas_dzasum
          ( $(blasint n)
          , (double*)$(openblas_complex_double* ptr)
          , $(blasint inc)
          )
        }
      |]

  nrm2 x =
    unsafePerformIO $
    unsafeWithV x $ \n ptr inc ->
      [C.exp|
        double {
          cblas_dznrm2
          ( $(blasint n)
          , (double*)$(openblas_complex_double* ptr)
          , $(blasint inc)
          )
        }
      |]

  amax x =
    unsafePerformIO $
    unsafeWithV x $ \n ptr inc ->
      [C.exp|
        blasint {
          cblas_izamax
          ( $(blasint n)
          , (double*)$(openblas_complex_double* ptr)
          , $(blasint inc)
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
          cblas_zaxpy
          ( $(blasint n)
          , (double*)$(openblas_complex_double* pa)
          , (double*)$(openblas_complex_double* ptrx)
          , $(blasint incx)
          , (double*)$(openblas_complex_double* ptry)
          , $(blasint incy)
          );
        }
      |]

  scal a x =
    unsafePrimToPrim $
    withV x $ \n ptr inc ->
    with a $ \pa ->
      [C.block|
        void {
          cblas_zscal
          ( $(blasint n)
          , (double*)$(openblas_complex_double* pa)
          , (double*)$(openblas_complex_double* ptr)
          , $(blasint inc)
          );
        }
      |]

  copy x y =
    unsafePrimToPrim $
    unsafeWithV x $ \n ptrx incx ->
    withV y $ \_ ptry incy ->
      [C.exp|
        void {
          cblas_zcopy
          ( $(blasint n)
          , (double*)$(openblas_complex_double* ptrx)
          , $(blasint incx)
          , (double*)$(openblas_complex_double* ptry)
          , $(blasint incy)
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
          ( $(blasint n)
          , (double*)$(openblas_complex_double* ptrx)
          , $(blasint incx)
          , (double*)$(openblas_complex_double* ptry)
          , $(blasint incy)
          )
        }
      |]

