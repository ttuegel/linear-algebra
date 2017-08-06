module Internal.Vector
    ( V(..), Storable
    , unsafeIndex, unsafeIndexPrim, unsafeIndexM
    , new, thaw, freeze, copy
    , unsafeNew, unsafeFreeze, unsafeThaw
    , read, write, slice
    , unsafeRead, unsafeWrite
    , advanceForeignPtr
    ) where

import Control.Monad (when)
import Control.Monad.Primitive
import Foreign.ForeignPtr
       ( ForeignPtr, mallocForeignPtrArray, plusForeignPtr, withForeignPtr )
import Foreign.Storable (Storable, peekElemOff, pokeElemOff, sizeOf)
import Prelude hiding (read)

import Internal.Int
import Internal.Mut


data V (n :: Nat) a
  = V { vdim :: {-# UNPACK #-} !(N n)
      , vptr :: {-# UNPACK #-} !(ForeignPtr a)
      , vinc :: {-# UNPACK #-} !I
      }

indexIO :: Storable a => V n a -> N n -> IO a
indexIO v (N i) = unsafeIndexIO v i

unsafeIndexIO :: Storable a => V n a -> I -> IO a
unsafeIndexIO (V {..}) i =
  withForeignPtr vptr $ \ptr ->
  peekElemOff ptr (fromIntegral (i * vinc))

indexPrim :: (PrimMonad m, Storable a) => V n a -> N n -> m a
indexPrim v (N i) = unsafeIndexPrim v i

unsafeIndexPrim :: (PrimMonad m, Storable a) => V n a -> I -> m a
unsafeIndexPrim v i = unsafePrimToPrim (unsafeIndexIO v i)

indexM :: (Monad m, Storable a) => V n a -> N n -> m a
indexM v (N i) = unsafeIndexM v i

unsafeIndexM :: (Monad m, Storable a) => V n a -> I -> m a
unsafeIndexM v i = (return . unsafeInlineIO) (unsafeIndexIO v i)

index :: Storable a => V n a -> N n -> a
index v (N i) = unsafeIndex v i

unsafeIndex :: Storable a => V n a -> I -> a
unsafeIndex v i = unsafeInlineIO (unsafeIndexIO v i)

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

read :: (PrimMonad m, Storable a) => Mut (V n) (PrimState m) a -> N i -> m a
read v (N i) = unsafeRead v i

unsafeRead :: (PrimMonad m, Storable a) =>
              Mut (V n) (PrimState m) a -> I -> m a
unsafeRead (Mut (V {..})) i =
  unsafePrimToPrim $ withForeignPtr vptr $ \ptr ->
    peekElemOff ptr (fromIntegral $! i * vinc)

write :: (PrimMonad m, Storable a) =>
         Mut (V n) (PrimState m) a -> N n -> a -> m ()
write v (N i) a = unsafeWrite v i a

unsafeWrite :: (PrimMonad m, Storable a) =>
               Mut (V n) (PrimState m) a -> I -> a -> m ()
unsafeWrite (Mut (V {..})) i a =
  unsafePrimToPrim $ withForeignPtr vptr $ \ptr ->
    pokeElemOff ptr (fromIntegral $! i * vinc) a

slice :: (Storable a, i < n, i + l <= n) =>
         N i -> N l -> Mut (V n) s a -> Mut (V l) s a
slice (N i) vdim' (Mut v) =
  Mut V { vdim = vdim'
        , vptr = advanceForeignPtr (vptr v) (fromIntegral $! i * vinc v)
        , vinc = vinc v
        }

copy :: (PrimMonad m, Storable a) => Mut (V n) (PrimState m) a -> V n a -> m ()
copy a@(Mut (V (N n) _ _)) b = do
  let
    copy_go !i = do
      unsafeIndexPrim b i >>= unsafeWrite a i
      when (i /= 0) (copy_go (i - 1))
  when (n > 0) (copy_go (n - 1))

advanceForeignPtr
  :: forall a. Storable a =>
     ForeignPtr a
  -> Int
  -> ForeignPtr a
advanceForeignPtr fptr n = plusForeignPtr fptr (n * size)
  where
    size = sizeOf (undefined :: a)
