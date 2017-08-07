{-# LANGUAGE TemplateHaskell #-}

module Internal.Vector
    ( V(..), bounds, Storable
    , new, unsafeNew, copy, slice, ecils
    , read, write
    , unsafeRead, unsafeWrite
    ) where

import Control.Monad (when)
import Control.Monad.ST
import Control.Monad.Primitive
import Foreign.ForeignPtr ( ForeignPtr, mallocForeignPtrArray, withForeignPtr )
import Foreign.Storable (Storable, peekElemOff, pokeElemOff, sizeOf)
import Prelude hiding (read)

import Internal.Int


data V s (n :: Nat) a
  = V { vdim :: {-# UNPACK #-} !(N n)
      , vptr :: {-# UNPACK #-} !(ForeignPtr a)
      , voff :: {-# UNPACK #-} !I
      , vinc :: {-# UNPACK #-} !I
      }

bounds :: V s n a -> (N 0, N n)
bounds (V {..}) = ($(known 0), vdim)

unsafeNew :: Storable a => I -> ST s (V s n a)
unsafeNew n =
  unsafePrimToST $ do
    vptr <- mallocForeignPtrArray (fromIntegral n)
    let
      vdim = N n
      voff = 0
      vinc = 1
    pure V {..}

new :: Storable a => N n -> ST s (V s n a)
new n = unsafeNew (fromN n)

read :: (Storable a, u <= n) => V s n a -> B l u -> ST s a
read v i = unsafeRead v (fromB i)

unsafeRead :: Storable a => V s n a -> I -> ST s a
unsafeRead (V {..}) i =
  unsafePrimToST $
  withForeignPtr vptr $ \ptr ->
  peekElemOff ptr (fromIntegral (i * vinc + voff))

write :: (Storable a, u <= n) => V s n a -> B l u -> a -> ST s ()
write v i a = unsafeWrite v (fromB i) a

unsafeWrite :: Storable a => V s n a -> I -> a -> ST s ()
unsafeWrite (V {..}) i a =
  unsafePrimToST $
  withForeignPtr vptr $ \ptr ->
  pokeElemOff ptr (fromIntegral (i * vinc + voff)) a

slice :: (Storable a, i <= n, i + d * l <= n) => N i -> N l -> N d -> V s n a -> V s l a
slice i vdim' vinc' v =
  V { vdim = vdim'
    , vptr = vptr v
    , voff = voff v + fromN i * vinc v
    , vinc = fromN vinc' * vinc v
    }

ecils :: (Storable a, i <= n, i + d * l <= n + 1) =>
         B 1 i  -- ^ index @b@, interpreted as a negative offset from @n@
      -> N l  -- ^ length $l$ of result
      -> N d  -- ^ stride $d$ of result, interpreted as a negative number
      -> V s n a
      -> V s l a
ecils i vdim' vinc' v =
  V { vdim = vdim'
    , vptr = vptr v
    , voff = voff v + (fromN (vdim v) - fromB i) * vinc v
    , vinc = negate (fromN vinc') * vinc v
    }

copy :: Storable a =>
        V s n a  -- ^ destination
     -> V s n a  -- ^ source
     -> ST s ()
copy dst src = do
  let
    copy_go !i = do
      unsafeRead src i >>= unsafeWrite dst i
      when (i /= 0) (copy_go (i - 1))
    n = fromN (vdim dst)
  when (n > 0) (copy_go (n - 1))
