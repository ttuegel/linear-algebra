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
import Internal.Mut


data V (n :: Dim) a
  = V
    {-# UNPACK #-} !I
    {-# UNPACK #-} !(ForeignPtr a)
    {-# UNPACK #-} !I

class Vec v where
  dimV :: v a -> I

instance Vec v => Vec (Mut v s) where
  dimV (Mut v) = dimV v

instance Vec (V n) where
  dimV (V n _ _) = n

type instance Mutable (V n) = Mut (V n)

instance Storable a => MVector (Mut (V n)) a where
  {-# INLINE basicLength #-}
  basicLength (Mut (V n _ _)) = fromIntegral n

  {-# INLINE basicUnsafeSlice #-}
  basicUnsafeSlice start len (Mut (V _ fptr inc)) =
    let off = start * fromIntegral inc in
      Mut (V (fromIntegral len) (advanceForeignPtr fptr off) inc)

  {-# INLINE basicOverlaps #-}
  basicOverlaps (Mut (V lenx ptrx incx)) (Mut (V leny ptry incy)) =
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
        ptr <- mallocForeignPtrArray len
        pure $ Mut (V (fromIntegral len) ptr 1)
    where
      maxI = fromIntegral (maxBound :: I) :: Int
      size = sizeOf (undefined :: Double)
      mx = maxI `quot` size

  {-# INLINE basicInitialize #-}
  basicInitialize (Mut (V len fptr inc)) =
    unsafePrimToPrim $
    withForeignPtr fptr $ \ptr -> do
      let
        end = fromIntegral (inc * len)
        inc_ = fromIntegral inc
        sz = sizeOf (undefined :: Double)

        basicInitialize_go ix
          | ix >= end = pure ()
          | otherwise = do
              fillBytes (advancePtr ptr ix) 0 sz
              basicInitialize_go (ix + inc_)

      basicInitialize_go 0

  {-# INLINE basicUnsafeRead #-}
  basicUnsafeRead (Mut (V _ fptr inc)) ix =
    unsafePrimToPrim $
      withForeignPtr fptr $ \ptr ->
        peekElemOff ptr (ix * fromIntegral inc)

  {-# INLINE basicUnsafeWrite #-}
  basicUnsafeWrite (Mut (V _ fptr inc)) ix a = do
    unsafePrimToPrim $
      withForeignPtr fptr $ \ptr ->
        pokeElemOff ptr (ix * fromIntegral inc) a

instance Storable a => Vector (V n) a where
  {-# INLINE basicUnsafeFreeze #-}
  basicUnsafeFreeze (Mut v) = pure v

  {-# INLINE basicUnsafeThaw #-}
  basicUnsafeThaw v = pure (Mut v)

  {-# INLINE basicLength #-}
  basicLength (V len _ _) = fromIntegral len

  {-# INLINE basicUnsafeSlice #-}
  basicUnsafeSlice start len (V _ fptr inc) =
    let off = start * fromIntegral inc in
      V (fromIntegral len) (advanceForeignPtr fptr off) inc

  {-# INLINE basicUnsafeIndexM #-}
  basicUnsafeIndexM (V _ fptr inc) ix =
    return . unsafeInlineIO $
      withForeignPtr fptr $ \ptr ->
        peekElemOff ptr (ix * fromIntegral inc)

  {-# INLINE elemseq #-}
  elemseq _ = seq

advanceForeignPtr
  :: forall a. Storable a =>
     ForeignPtr a
  -> Int
  -> ForeignPtr a
advanceForeignPtr fptr n = plusForeignPtr fptr (n * size)
  where
    size = sizeOf (undefined :: a)

unsafeWithV :: V n a -> (I -> Ptr a -> I -> IO b) -> IO b
unsafeWithV (V len fptr inc) cont =
  withForeignPtr fptr $ \ptr -> cont len ptr inc

withV :: Mut (V n) s a -> (I -> Ptr a -> I -> IO b) -> IO b
withV (Mut (V len fptr inc)) cont =
  withForeignPtr fptr $ \ptr -> cont len ptr inc
