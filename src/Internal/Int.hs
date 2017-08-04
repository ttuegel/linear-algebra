{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}

module Internal.Int where

import GHC.TypeLits (Nat)
import Language.Haskell.TH hiding (dyn)

import qualified GHC.TypeLits as GHC

#ifdef INT

#if INT == int64_t

import Data.Int (Int64)

type I = Int64

#else

import Data.Int (Int32)

type I = Int32

#endif

newtype N (n :: Dim) = N { toI :: I }
  deriving Show

data Dim where
  Sta :: Nat -> Dim
  Dyn :: Dim

type family (+) (a :: Dim) (b :: Dim) :: Dim where
  (+) ('Sta m) ('Sta n) = 'Sta (m GHC.+ n)
  (+) 'Dyn _ = 'Dyn
  (+) _ 'Dyn = 'Dyn

class (<) (a :: Dim) (b :: Dim) where
  lessThan :: N a -> N b -> Bool

instance ((m GHC.+ 1) GHC.<= n) => (<) ('Sta m) ('Sta n) where
  {-# INLINE lessThan #-}
  lessThan _ _ = True

instance (<) 'Dyn a where
  {-# INLINE lessThan #-}
  lessThan (N a) (N b) = a < b

instance (<) a 'Dyn where
  {-# INLINE lessThan #-}
  lessThan (N a) (N b) = a < b

class (<=) (a :: Dim) (b :: Dim) where
  lessThanOrEqual :: N a -> N b -> Bool

instance (m GHC.<= n) => (<=) ('Sta m) ('Sta n) where
  {-# INLINE lessThanOrEqual #-}
  lessThanOrEqual _ _ = True

instance (<=) ('Sta n) 'Dyn where
  {-# INLINE lessThanOrEqual #-}
  lessThanOrEqual (N a) (N b) = a <= b

instance (<=) 'Dyn a where
  {-# INLINE lessThanOrEqual #-}
  lessThanOrEqual (N a) (N b) = a <= b

plus :: N a -> N b -> N (a + b)
plus (N a) (N b) = N (a + b)

sta :: Integer -> Q Exp
sta i = [| N $(litE (integerL i)) :: N ('Sta $(litT (numTyLit i))) |]

dyn :: I -> N 'Dyn
dyn = N

#endif
