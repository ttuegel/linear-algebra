{-# LANGUAGE CPP #-}

module Internal.Int where

#ifdef INT

#if INT == "int64_t"

import Data.Int (Int64)

type I = Int64

#elif INT == "int32_t"

import Data.Int (Int32)

type I = Int32

#endif

#else

import Data.Int (Int32)

type I = Int32

#endif
