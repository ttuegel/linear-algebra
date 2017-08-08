{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Monad (void)
import Control.Monad.ST.Strict
import Hedgehog

import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Data.Int.Blas
import Data.Vector.Blas as V
import Numeric.LinearAlgebra.Blas

equiv :: Double  -- ^ size
      -> Double  -- ^ test value
      -> Double  -- ^ true value
      -> Bool
equiv sz comp true = abs sz + abs (sfac * (comp - true)) - sz == 0
  where
    sfac = 9.765625E-4

prop_nrm2_empty_lit :: Property
prop_nrm2_empty_lit = property $ do
  let
    t = runST $ do
      v :: V s 0 Double <- $(litV ([] :: [Double]))
      nrm2 v
  annotateShow t
  assert $ equiv 0.0 t 0.0

prop_nrm2_empty :: Property
prop_nrm2_empty = property $ do
  let
    t = runST $ do
      v :: V s 0 Double <- V.empty
      nrm2 v
  annotateShow t
  assert $ equiv 0.0 t 0.0

prop_nrm2_empty_slice :: Property
prop_nrm2_empty_slice = property $ do
  as <- forAll $ Gen.list (Range.singleton 10) (Gen.double (Range.constant (-1) 1))
  let
    t = runST $ do
      vlong :: V s 10 Double <- V.unsafeFromList $(known 10) as
      let v = V.slice $(known 0) $(known 0) $(known 1) vlong
      nrm2 v
  annotateShow t
  assert $ equiv 0.0 t 0.0

prop_nrm2_singleton :: Property
prop_nrm2_singleton = property $ do
  a <- forAll $ Gen.double (Range.constant (-1) 1)
  let
    t = runST $ do
      v :: V s 1 Double <- V.singleton a
      nrm2 v
  annotateShow t
  assert $ equiv 0.0 t (abs a)

prop_nrm2_singleton_slice :: Property
prop_nrm2_singleton_slice = property $ do
  as <- forAll $ Gen.list (Range.singleton 10) (Gen.double (Range.constant (-1) 1))
  let
    t = runST $ do
      vlong :: V s 10 Double <- V.unsafeFromList $(known 10) as
      let v = V.slice $(known 0) $(known 1) $(known 1) vlong
      nrm2 v
  annotateShow t
  assert $ equiv 0.0 t (abs (head as))


main :: IO ()
main = void $ checkParallel $$(discover)
