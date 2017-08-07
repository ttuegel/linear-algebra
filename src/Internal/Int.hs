{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}

module Internal.Int
    ( I, N(..), B(..), enumN, enumB, minB, maxB
    , known, bounded, fromProxy, toProxy
    , type (<)
    , module GHC.TypeLits
    ) where

import Data.Proxy
import GHC.TypeLits
import Language.Haskell.TH hiding (dyn)

#ifdef USE_INT64

import Data.Int (Int64)

type I = Int64

#else

import Data.Int (Int32)

type I = Int32

#endif

class (a + 1 <= b) => (<) (a :: Nat) (b :: Nat)
instance (a + 1 <= b) => (<) (a :: Nat) (b :: Nat)

-- | @N n@ is a newtype wrapper of 'I' such that the wrapped integer
-- @i :: I@ is at least zero, but less than the type index @n :: Nat@.
newtype N (n :: Nat) = N { fromN :: I }
  deriving (Eq, Show)

-- | @B l u@ is a newtype wrapper of @i :: I@ such that
-- @l :: Nat <= i < u :: Nat@.
newtype B (l :: Nat) (u :: Nat) = B { fromB :: I }
  deriving (Eq, Show)

known :: Integer -> Q Exp
known i = [| N $(litE (integerL i)) :: N $(litT (numTyLit i)) |]

bounded :: (l <= i, i < u) => N i -> n l -> n u -> B l u
bounded i _ _ = B (fromN i)

fromProxy :: KnownNat n => proxy n -> N n
fromProxy p = N (fromIntegral (natVal p))

toProxy :: N n -> Proxy n
toProxy _ = Proxy

data Step s a where
  Yield :: a -> s -> Step s a
  Skip :: s -> Step s a
  Done :: Step s a

instance Functor (Step s) where
  fmap f (Yield a s) = Yield (f a) s
  fmap _ (Skip s) = Skip s
  fmap _ Done = Done

data It a = forall s. It (s -> Step s a) s

instance Functor It where
  fmap f (It step s0) = It (fmap f . step) s0

instance Foldable It where
  foldMap f (It step _s) = foldMap_go _s where
    foldMap_go _s =
      case step _s of
        Yield a _s -> mappend (f a) (foldMap_go _s)
        Skip _s -> foldMap_go _s
        Done -> mempty

enumN :: N n -> It (B 0 n)
enumN (fromN -> n) = It step 0 where
  step s
    | s < n = Yield (B s) (s + 1)
    | otherwise = Done

enumB :: (l <= u) => N l -> N u -> It (B l u)
enumB (fromN -> l) (fromN -> u) = It step l where
  step s
    | s < u = Yield (B s) (s + 1)
    | otherwise = Done

maxB :: (l <= u) => (N l, N u) -> B l u
maxB (_, N u) = B (u - 1)

minB :: (l <= u) => (N l, N u) -> B l u
minB (N l, _) = B l
