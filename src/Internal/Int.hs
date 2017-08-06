{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}

module Internal.Int
    ( I, N(..), toI
    , known, bounded
    , type (<)
    , module GHC.TypeNats
    ) where

import GHC.TypeNats
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
newtype N (n :: Nat) = N { toI :: I }
  deriving (Eq, Show)

known :: Integer -> Q Exp
known i = [| N $(litE (integerL i)) :: N $(litT (numTyLit i)) |]

bounded :: Integer -> Integer -> Q Exp
bounded i n
  | i < n = [| N $(litE (integerL i)) :: N $(litT (numTyLit n)) |]
  | otherwise =
      fail ("Argument i = " ++ show i ++ " must be less than bound n = "
             ++ show n)
