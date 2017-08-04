module Internal.Vector
    ( V(..), Storable
    , unsafeIndex, unsafeIndexPrim, unsafeIndexM
    , new, thaw, freeze, copy
    , unsafeNew, unsafeFreeze, unsafeThaw
    , read, write, slice
    , unsafeRead, unsafeWrite, unsafeSlice
    , advanceForeignPtr
    ) where

import Control.Monad (when)
import Control.Monad.Primitive
import Foreign.ForeignPtr
       ( ForeignPtr, mallocForeignPtrArray, plusForeignPtr, withForeignPtr )
import Foreign.Marshal.Array (advancePtr)
import Foreign.Marshal.Utils (fillBytes)
import Foreign.Ptr (Ptr)
import Foreign.Storable (Storable, peekElemOff, pokeElemOff, sizeOf)
import Prelude hiding (read)

import Internal.Int
import Internal.Mut


data V (n :: Dim) a
  = V
    {-# UNPACK #-} !(N n)
    {-# UNPACK #-} !(ForeignPtr a)
    {-# UNPACK #-} !I

unsafeIndex :: Storable a => V n a -> I -> a
unsafeIndex (V _ fptr inc) !i =
  unsafeInlineIO $ withForeignPtr fptr $ \ptr ->
    peekElemOff ptr (fromIntegral $! i * inc)

unsafeIndexPrim :: (PrimMonad m, Storable a) => V n a -> I -> m a
unsafeIndexPrim (V _ fptr inc) !i =
  unsafePrimToPrim $ withForeignPtr fptr $ \ptr ->
    peekElemOff ptr (fromIntegral $! i * inc)

unsafeIndexM :: (Monad m, Storable a) => V n a -> I -> m a
unsafeIndexM (V _ fptr inc) !i =
  return $ unsafeInlineIO $ withForeignPtr fptr $ \ptr ->
    peekElemOff ptr (fromIntegral $! i * inc)

unsafeNew :: (PrimMonad m, Storable a) => I -> m (Mut (V n) (PrimState m) a)
unsafeNew n = unsafePrimToPrim $ do
  fp <- mallocForeignPtrArray (fromIntegral n)
  pure (Mut (V (N n) fp 1))

new :: (PrimMonad m, Storable a) => N n -> m (Mut (V n) (PrimState m) a)
new (N n) = unsafeNew n

unsafeThaw :: PrimMonad m => V n a -> m (Mut (V n) (PrimState m) a)
unsafeThaw = pure . Mut

thaw :: (PrimMonad m, Storable a) => V n a -> m (Mut (V n) (PrimState m) a)
thaw as@(V n _ _) = do
  bs <- new n
  copy bs as
  pure bs

unsafeFreeze :: PrimMonad m => Mut (V n) (PrimState m) a -> m (V n a)
unsafeFreeze (Mut v) = pure v

freeze :: (PrimMonad m, Storable a) => Mut (V n) (PrimState m) a -> m (V n a)
freeze as@(Mut (V n _ _)) = do
  bs <- new n
  let
    freeze_go !i = do
      unsafeRead as i >>= unsafeWrite bs i
      when (i /= 0) (freeze_go (i - 1))
  when (toI n > 0) (freeze_go (toI n - 1))
  case bs of
    Mut v -> pure v

unsafeRead :: (PrimMonad m, Storable a) => Mut (V n) (PrimState m) a -> I -> m a
unsafeRead (Mut (V _ fptr inc)) i =
  unsafePrimToPrim $ withForeignPtr fptr $ \ptr ->
    peekElemOff ptr (fromIntegral $! i * inc)

read :: (PrimMonad m, Storable a, i < n) =>
        Mut (V n) (PrimState m) a -> N i -> m a
read v@(Mut (V nn _ _)) ni@(N i)
  | lessThan ni nn = unsafeRead v i
  | otherwise =
      error ("read: index `" ++ show ni
              ++ "' out of bounds `" ++ show nn ++ "'")

unsafeWrite :: (PrimMonad m, Storable a) =>
               Mut (V n) (PrimState m) a -> I -> a -> m ()
unsafeWrite (Mut (V _ fptr inc)) i a =
  unsafePrimToPrim $ withForeignPtr fptr $ \ptr ->
    pokeElemOff ptr (fromIntegral $! i * inc) a

write :: (PrimMonad m, Storable a, i < n) =>
         Mut (V n) (PrimState m) a -> N i -> a -> m ()
write v@(Mut (V nn _ _)) ni@(N i) a
  | lessThan ni nn = unsafeWrite v i a
  | otherwise =
      error ("write: index `" ++ show ni
              ++ "' out of bounds `" ++ show nn ++ "'")

slice :: (Storable a, i < n, i + l <= n) =>
         N i -> N l -> Mut (V n) s a -> Mut (V l) s a
slice ni@(N i) nl v@(Mut (V nn _ _))
  | lessThan ni nn && lessThanOrEqual (plus ni nl) nn =
      unsafeSlice i nl v
  | otherwise =
      error ("slice (" ++ show ni ++ ", " ++ show nl
              ++ ") out of bounds `" ++ show nn ++ "'")

copy :: (PrimMonad m, Storable a) => Mut (V n) (PrimState m) a -> V n a -> m ()
copy a@(Mut (V (N n) _ _)) b = do
  let
    copy_go !i = do
      unsafeIndexPrim b i >>= unsafeWrite a i
      when (i /= 0) (copy_go (i - 1))
  when (n > 0) (copy_go (n - 1))

unsafeSlice :: Storable a => I -> N l -> Mut (V n) s a -> Mut (V l) s a
unsafeSlice i nl (Mut (V _ fptr inc)) =
  let fptr' = advanceForeignPtr fptr (fromIntegral $! i * inc) in
    Mut (V nl fptr' inc)

advanceForeignPtr
  :: forall a. Storable a =>
     ForeignPtr a
  -> Int
  -> ForeignPtr a
advanceForeignPtr fptr n = plusForeignPtr fptr (n * size)
  where
    size = sizeOf (undefined :: a)
