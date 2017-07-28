module Data.Dim where

import GHC.TypeLits


data Dim where
  Sta :: Nat -> Dim
  Dyn :: Dim

type family (:+) (a :: Dim) (b :: Dim) :: Dim where
  (:+) ('Sta m) ('Sta n) = 'Sta (m + n)
  (:+) 'Dyn _ = 'Dyn
  (:+) _ 'Dyn = 'Dyn
