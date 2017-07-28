module Internal.Vector where

import Control.Monad.Primitive
import Foreign.ForeignPtr
       ( ForeignPtr, mallocForeignPtrArray, plusForeignPtr, withForeignPtr )
import Foreign.Marshal.Array (advancePtr)
import Foreign.Marshal.Utils (fillBytes)
import Foreign.Ptr (Ptr)
import Foreign.Storable (Storable, peekElemOff, pokeElemOff, sizeOf)
import Data.Vector.Generic (Mutable, Vector)
import Data.Vector.Generic.Mutable (MVector(..))

import qualified Data.Vector.Generic as Vector

import Data.Dim
import Data.Int.Blas


data Vmut (n :: Dim) s a
  = Vmut
    {-# UNPACK #-} !I  -- len
    {-# UNPACK #-} !(ForeignPtr a)
    {-# UNPACK #-} !I  -- inc

data V (n :: Dim) a
  = V
    {-# UNPACK #-} !I  -- len
    {-# UNPACK #-} !(ForeignPtr a)
    {-# UNPACK #-} !I  -- inc

type instance Mutable (V n) = (Vmut n)

instance Storable a => MVector (Vmut n) a where
  {-# INLINE basicLength #-}
  basicLength (Vmut len _ _) = fromIntegral len

  {-# INLINE basicUnsafeSlice #-}
  basicUnsafeSlice start len (Vmut _ ptr inc) =
    let ptr' = advanceForeignPtr ptr (start * fromIntegral inc) in
      Vmut (fromIntegral len) ptr' inc

  {-# INLINE basicOverlaps #-}
  basicOverlaps (Vmut lenx ptrx incx) (Vmut leny ptry incy) =
      between ptrx ptry (leny * incy)
      || between ptry ptrx (lenx * incx)
    where
      between x y n = x >= y && x < advanceForeignPtr y (fromIntegral n)

  {-# INLINE basicUnsafeNew #-}
  basicUnsafeNew len
    | len < 0 =
        error ("Blas.basicUnsafeNew: negative length: " ++ show len)
    | len > mx =
        error ("Blas.basicUnsafeNew: length too large: " ++ show len)
    | otherwise = unsafePrimToPrim $ do
        fptr <- mallocForeignPtrArray len
        pure (Vmut (fromIntegral len) fptr 1)
    where
      maxI = fromIntegral (maxBound :: I) :: Int
      size = sizeOf (undefined :: a)
      mx = maxI `quot` size

  {-# INLINE basicInitialize #-}
  basicInitialize x =
    unsafePrimToPrim $
    withVmut x $ \len ptr inc -> do
      let
        end = fromIntegral (inc * len)
        inc_ = fromIntegral inc
        sz = sizeOf (undefined :: a)
      let basicInitialize_go ix
            | ix >= end = pure ()
            | otherwise = do
                fillBytes (advancePtr ptr ix) 0 sz
                basicInitialize_go (ix + inc_)
      basicInitialize_go 0

  {-# INLINE basicUnsafeRead #-}
  basicUnsafeRead x ix =
    unsafePrimToPrim $
    withVmut x $ \_ ptr inc ->
      peekElemOff ptr (ix * fromIntegral inc)

  {-# INLINE basicUnsafeWrite #-}
  basicUnsafeWrite x ix a =
    unsafePrimToPrim $
    withVmut x $ \_ ptr inc ->
      pokeElemOff ptr (ix * fromIntegral inc) a

instance Storable a => Vector (V n) a where
  {-# INLINE basicUnsafeFreeze #-}
  basicUnsafeFreeze (Vmut len ptr inc) =
    pure (V len ptr inc)

  {-# INLINE basicUnsafeThaw #-}
  basicUnsafeThaw (V len ptr inc) =
    pure (Vmut len ptr inc)

  {-# INLINE basicLength #-}
  basicLength (V len _ _) = fromIntegral len

  {-# INLINE basicUnsafeSlice #-}
  basicUnsafeSlice start len (V _ ptr inc) =
    let ptr' = advanceForeignPtr ptr (start * fromIntegral inc) in
      V (fromIntegral len) ptr' inc

  {-# INLINE basicUnsafeIndexM #-}
  basicUnsafeIndexM (V _ fptr inc) ix =
    pure . unsafeInlineIO $ withForeignPtr fptr $ \ptr ->
      peekElemOff ptr (ix * fromIntegral inc)

  {-# INLINE basicUnsafeCopy #-}
  basicUnsafeCopy y x =
    unsafePrimToPrim $
    withVmut y $ \len ptry incy ->
    unsafeWithV x $ \_ ptrx incx -> do
      let
        end = fromIntegral (incy * len)
        incx_ = fromIntegral incx
        incy_ = fromIntegral incy
      let basicUnsafeCopy_go ixx ixy
            | ixy >= end = pure ()
            | otherwise = do
                a <- peekElemOff ptrx ixx
                pokeElemOff ptry ixy a
                basicUnsafeCopy_go (ixx + incx_) (ixy + incy_)
      basicUnsafeCopy_go 0 0

unsafeWithV :: Storable a => V n a -> (I -> Ptr a -> I -> IO b) -> IO b
unsafeWithV (V len fptr inc) cont =
  withForeignPtr fptr $ \ptr -> cont len ptr inc

withVmut
  :: Storable a =>
     Vmut n s a
  -> (I -> Ptr a -> I -> IO b)
  -> IO b
withVmut (Vmut len fptr inc) cont =
  withForeignPtr fptr $ \ptr -> cont len ptr inc

advanceForeignPtr
  :: forall a. Storable a =>
     ForeignPtr a
  -> Int
  -> ForeignPtr a
advanceForeignPtr fptr n = plusForeignPtr fptr (n * size)
  where
    size = sizeOf (undefined :: a)
