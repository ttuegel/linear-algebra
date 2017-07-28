{-# LANGUAGE TemplateHaskell #-}

module Language.C.Inline.Context.Blas where

import Data.Complex
import Language.C.Inline.Context (Context(..))

import qualified Data.Map as Map
import qualified Language.C.Types as C

import Internal.Int

blasCtx :: Context
blasCtx =
  mempty
  { ctxTypesTable =
      Map.fromList
      [ (C.TypeName "blasint", [t| I |])
      , (C.Double, [t| Double |])
      , (C.TypeName "openblas_complex_double", [t| Complex Double |])
      ]
  }
