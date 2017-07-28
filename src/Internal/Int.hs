{-# LANGUAGE CPP #-}

module Internal.Int where

#ifdef OPENBLAS_USE64BITINT
import Data.Int (Int64)
#else
import Data.Int (Int32)
#endif


#ifdef OPENBLAS_USE64BITINT
type I = Int64
#else
type I = Int32
#endif
