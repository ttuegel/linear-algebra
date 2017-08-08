{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Monad (void)
import Control.Monad.ST.Strict
import Hedgehog

import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Data.Int.Blas
import Data.Vector.Blas (V, litV)
import Numeric.LinearAlgebra.Blas

import qualified Data.Vector.Blas as V

dnrm2 :: V s n Double -> ST s Double
dnrm2 = nrm2

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

prop_nrm2_reverse :: Property
prop_nrm2_reverse = property $ do
  as <- forAll $ Gen.list (Range.singleton 10) (Gen.double (Range.constant (-1) 1))
  let
    (s, t) = runST $ do
      v :: V s 10 Double <- V.unsafeFromList $(known 10) as
      v' <- V.reverse v
      (,) <$> nrm2 v <*> nrm2 v'
  annotateShow s
  annotateShow t
  assert $ equiv 0.0 s t

prop_fromList_toList :: Property
prop_fromList_toList = property $ do
  as <- forAll $ Gen.list (Range.singleton 10) (Gen.double (Range.constant (-1) 1))
  let
    as' = runST $ do
      v :: V s 10 Double <- V.fromList $(known 10) as
      V.toList v
  annotateShow as'
  as === as'

prop_reverse_reverse :: Property
prop_reverse_reverse = property $ do
  as <- forAll $ Gen.list (Range.singleton 10) (Gen.double (Range.constant (-1) 1))
  let
    as' = runST $ do
      v :: V s 10 Double <- V.fromList $(known 10) as
      V.toList =<< V.reverse =<< V.reverse v
  annotateShow as'
  as === as'

prop_nrm2_known :: Property
prop_nrm2_known = property $ do
  let
    n1s = runST $ do
      dv1 <- V.slice $(known 0) $(known 0) $(known 1)
             <$> $(litV [0.1E0, 2.0E0, 2.0E0, 2.0E0, 2.0E0, 2.0E0, 2.0E0, 2.0E0 :: Double])
      dv2 <- V.slice $(known 0) $(known 1) $(known 1)
             <$> $(litV [0.3E0, 3.0E0, 3.0E0, 3.0E0, 3.0E0, 3.0E0, 3.0E0, 3.0E0 :: Double])
      dv3 <- V.slice $(known 0) $(known 2) $(known 1)
             <$> $(litV [0.3E0, -0.4E0, 4.0E0, 4.0E0, 4.0E0, 4.0E0, 4.0E0, 4.0E0 :: Double])
      dv4 <- V.slice $(known 0) $(known 3) $(known 1)
             <$> $(litV [0.2E0, -0.6E0, 0.3E0, 5.0E0, 5.0E0, 5.0E0, 5.0E0, 5.0E0 :: Double])
      dv5 <- V.slice $(known 0) $(known 4) $(known 1)
             <$> $(litV [0.1E0, -0.3E0, 0.5E0, -0.1E0, 6.0E0, 6.0E0, 6.0E0, 6.0E0 :: Double])
      sequence
        [ dnrm2 dv1
        , dnrm2 dv2
        , dnrm2 dv3
        , dnrm2 dv4
        , dnrm2 dv5
        ]
    n2s = runST $ do
      dv1 <- V.slice $(known 0) $(known 0) $(known 2)
             <$> $(litV [0.1E0, 8.0E0, 8.0E0, 8.0E0, 8.0E0, 8.0E0, 8.0E0, 8.0E0 :: Double])
      dv2 <- V.slice $(known 0) $(known 1) $(known 2)
             <$> $(litV [0.3E0, 9.0E0, 9.0E0, 9.0E0, 9.0E0, 9.0E0, 9.0E0, 9.0E0 :: Double])
      dv3 <- V.slice $(known 0) $(known 2) $(known 2)
             <$> $(litV [0.3E0, 2.0E0, -0.4E0, 2.0E0, 2.0E0, 2.0E0, 2.0E0, 2.0E0 :: Double])
      dv4 <- V.slice $(known 0) $(known 3) $(known 2)
             <$> $(litV [0.2E0, 3.0E0, -0.6E0, 5.0E0, 0.3E0, 2.0E0, 2.0E0, 2.0E0 :: Double])
      dv5 <- V.slice $(known 0) $(known 4) $(known 2)
             <$> $(litV [0.1E0, 4.0E0, -0.3E0, 6.0E0, -0.5E0, 7.0E0, -0.1E0, 3.0E0 :: Double])
      sequence
        [ dnrm2 dv1
        , dnrm2 dv2
        , dnrm2 dv3
        , dnrm2 dv4
        , dnrm2 dv5
        ]
    true = [0.0E0, 0.3E0, 0.5E0, 0.7E0, 0.6E0]
  annotateShow n1s
  assert $ and (zipWith (\c t -> equiv t c t) n1s true)
  annotateShow n2s
  assert $ and (zipWith (\c t -> equiv t c t) n2s true)

main :: IO ()
main = void $ checkParallel $$(discover)
