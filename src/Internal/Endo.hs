module Internal.Endo where

import Data.Semigroup (Semigroup (..))


newtype Endo m a = Endo { getEndo :: a -> m a }

instance Monad m => Semigroup (Endo m a) where
  (<>) a b = Endo (\r -> getEndo a r >>= getEndo b)

instance Monad m => Monoid (Endo m a) where
  mempty = Endo pure
  mappend = (<>)
