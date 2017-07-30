module Internal.Mut where

newtype Mut f s a = Mut (f a)
