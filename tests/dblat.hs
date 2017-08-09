{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Monad (void)
import Control.Monad.ST.Strict
import Foreign.C.Types (CSize)
import Hedgehog

import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Data.Int.Blas
import Data.Vector.Blas (V, litV)
import Numeric.LinearAlgebra.Blas

import qualified Data.Vector.Blas as V

dnrm2 :: V s n Double -> ST s Double
dnrm2 = nrm2

dasum :: V s n Double -> ST s Double
dasum = asum

idamax :: V s n Double -> ST s CSize
idamax = iamax

dscal :: Double -> V s n Double -> ST s ()
dscal = scal

equiv :: Double  -- ^ test value
      -> Double  -- ^ true value
      -> Bool
equiv comp true = abs sz + abs (sfac * (comp - true)) - sz == 0
  where
    sfac = 9.765625E-4
    sz = true

prop_nrm2_empty_lit :: Property
prop_nrm2_empty_lit = property $ do
  let
    t = runST $ do
      v <- $(litV ([] :: [Double]))
      dnrm2 v
  annotateShow t
  assert $ equiv t 0.0

prop_nrm2_empty :: Property
prop_nrm2_empty = property $ do
  let
    t = runST $ do
      v <- V.empty
      dnrm2 v
  annotateShow t
  assert $ equiv t 0.0

prop_nrm2_empty_slice :: Property
prop_nrm2_empty_slice = property $ do
  as <- forAll $ Gen.list (Range.singleton 10) (Gen.double (Range.constant (-1) 1))
  let
    t = runST $ do
      vlong <- V.unsafeFromList $(known 10) as
      let v = V.slice $(known 0) $(known 0) $(known 1) vlong
      dnrm2 v
  annotateShow t
  assert $ equiv t 0.0

prop_nrm2_singleton :: Property
prop_nrm2_singleton = property $ do
  a <- forAll $ Gen.double (Range.constant (-1) 1)
  let
    t = runST $ do
      v <- V.singleton a
      dnrm2 v
  annotateShow t
  assert $ equiv t (abs a)

prop_nrm2_singleton_slice :: Property
prop_nrm2_singleton_slice = property $ do
  as <- forAll $ Gen.list (Range.singleton 10) (Gen.double (Range.constant (-1) 1))
  let
    t = runST $ do
      vlong <- V.unsafeFromList $(known 10) as
      let v = V.slice $(known 0) $(known 1) $(known 1) vlong
      dnrm2 v
  annotateShow t
  assert $ equiv t (abs (head as))

prop_nrm2_reverse :: Property
prop_nrm2_reverse = property $ do
  as <- forAll $ Gen.list (Range.singleton 10) (Gen.double (Range.constant (-1) 1))
  let
    (s, t) = runST $ do
      v <- V.unsafeFromList $(known 10) as
      v' <- V.reverse v
      (,) <$> dnrm2 v <*> dnrm2 v'
  annotateShow s
  annotateShow t
  assert $ equiv s t

prop_fromList_toList :: Property
prop_fromList_toList = property $ do
  as <- forAll $ Gen.list (Range.singleton 10) (Gen.double (Range.constant (-1) 1))
  let
    as' = runST $ do
      v <- V.fromList $(known 10) as
      V.toList v
  annotateShow as'
  as === as'

prop_reverse_reverse :: Property
prop_reverse_reverse = property $ do
  as <- forAll $ Gen.list (Range.singleton 10) (Gen.double (Range.constant (-1) 1))
  let
    as' = runST $ do
      v <- V.fromList $(known 10) as
      V.toList =<< V.reverse =<< V.reverse v
  annotateShow as'
  as === as'

data Known s
  = Known
  { dv1 :: V s 0 Double
  , dv2 :: V s 1 Double
  , dv3 :: V s 2 Double
  , dv4 :: V s 3 Double
  , dv5 :: V s 4 Double
  }

knownData1 :: ST s (Known s)
knownData1 = do
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
  pure Known {..}

knownScaled1 :: ST s (Known s)
knownScaled1 = do
  dv1 <- V.slice $(known 0) $(known 0) $(known 1)
         <$> $(litV [0.10E0, 2.0E0, 2.0E0, 2.0E0, 2.0E0, 2.0E0, 2.0E0, 2.0E0 :: Double])
  dv2 <- V.slice $(known 0) $(known 1) $(known 1)
         <$> $(litV [-0.3E0, 3.0E0, 3.0E0, 3.0E0, 3.0E0, 3.0E0, 3.0E0, 3.0E0 :: Double])
  dv3 <- V.slice $(known 0) $(known 2) $(known 1)
         <$> $(litV [0.0E0, 0.0E0, 4.0E0, 4.0E0, 4.0E0, 4.0E0, 4.0E0, 4.0E0 :: Double])
  dv4 <- V.slice $(known 0) $(known 3) $(known 1)
         <$> $(litV [0.20E0, -0.60E0, 0.30E0, 5.0E0, 5.0E0, 5.0E0, 5.0E0, 5.0E0 :: Double])
  dv5 <- V.slice $(known 0) $(known 4) $(known 1)
         <$> $(litV [0.03E0, -0.09E0, 0.15E0, -0.03E0, 6.0E0, 6.0E0, 6.0E0, 6.0E0 :: Double])
  pure Known {..}

knownData2 :: ST s (Known s)
knownData2 = do
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
  pure Known {..}

knownScaled2 :: ST s (Known s)
knownScaled2 = do
  dv1 <- V.slice $(known 0) $(known 0) $(known 2)
         <$> $(litV [0.10E0, 8.0E0, 8.0E0, 8.0E0, 8.0E0, 8.0E0, 8.0E0, 8.0E0 :: Double])
  dv2 <- V.slice $(known 0) $(known 1) $(known 2)
         <$> $(litV [0.09E0, 9.0E0, 9.0E0, 9.0E0, 9.0E0, 9.0E0, 9.0E0, 9.0E0 :: Double])
  dv3 <- V.slice $(known 0) $(known 2) $(known 2)
         <$> $(litV [0.09E0, 2.0E0, -0.12E0, 2.0E0, 2.0E0, 2.0E0, 2.0E0, 2.0E0 :: Double])
  dv4 <- V.slice $(known 0) $(known 3) $(known 2)
         <$> $(litV [0.06E0, 3.0E0, -0.18E0, 5.0E0, 0.09E0, 2.0E0, 2.0E0, 2.0E0 :: Double])
  dv5 <- V.slice $(known 0) $(known 4) $(known 2)
         <$> $(litV [0.03E0, 4.0E0, -0.09E0, 6.0E0, -0.15E0, 7.0E0, -0.03E0, 3.0E0 :: Double])
  pure Known {..}

prop_nrm2_known :: Property
prop_nrm2_known = property $ do
  let
    comp :: (forall s. ST s (Known s)) -> [Double]
    comp dat = runST $ do
      Known {..} <- dat
      sequence
        [ dnrm2 dv1
        , dnrm2 dv2
        , dnrm2 dv3
        , dnrm2 dv4
        , dnrm2 dv5
        ]
    true = [0.0E0, 0.3E0, 0.5E0, 0.7E0, 0.6E0]
  comp knownData1 === true
  comp knownData2 === true

interleave :: (V s 5 Double, V s 5 Double) -> ST s (V s 5 Double, V s 5 Double)
interleave (as, bs) = do
  cs <- V.new $(known 10)
  let
    as' = V.slice $(known 0) $(known 5) $(known 2) cs
    bs' = V.slice $(known 1) $(known 5) $(known 2) cs
  copy as as'
  copy bs bs'
  pure (as', bs')

prop_nrm2_interleave :: Property
prop_nrm2_interleave = mk_prop_interleave dnrm2

prop_asum_known :: Property
prop_asum_known = property $ do
  let
    true = [0.0E0, 0.3E0, 0.7E0, 1.1E0, 1.0E0]
    comp :: (forall s. ST s (Known s)) -> [Double]
    comp dat = runST $ do
      Known {..} <- dat
      sequence
        [ dasum dv1
        , dasum dv2
        , dasum dv3
        , dasum dv4
        , dasum dv5
        ]
  comp knownData1 === true
  comp knownData2 === true

prop_asum_empty_lit :: Property
prop_asum_empty_lit = property $ do
  let
    t = runST $ do
      v <- $(litV ([] :: [Double]))
      dasum v
  annotateShow t
  assert $ equiv t 0.0

prop_asum_empty :: Property
prop_asum_empty = property $ do
  let
    t = runST $ do
      v <- V.empty
      dasum v
  annotateShow t
  assert $ equiv t 0.0

prop_asum_empty_slice :: Property
prop_asum_empty_slice = property $ do
  as <- forAll $ Gen.list (Range.singleton 10) (Gen.double (Range.constant (-1) 1))
  let
    t = runST $ do
      vlong <- V.unsafeFromList $(known 10) as
      let v = V.slice $(known 0) $(known 0) $(known 1) vlong
      dasum v
  annotateShow t
  assert $ equiv t 0.0

prop_asum_singleton :: Property
prop_asum_singleton = property $ do
  a <- forAll $ Gen.double (Range.constant (-1) 1)
  let
    t = runST $ do
      v <- V.singleton a
      dasum v
  annotateShow t
  assert $ equiv t (abs a)

prop_asum_singleton_slice :: Property
prop_asum_singleton_slice = property $ do
  as <- forAll $ Gen.list (Range.singleton 10) (Gen.double (Range.constant (-1) 1))
  let
    t = runST $ do
      vlong <- V.unsafeFromList $(known 10) as
      let v = V.slice $(known 0) $(known 1) $(known 1) vlong
      dasum v
  annotateShow t
  assert $ equiv t (abs (head as))

prop_asum_reverse :: Property
prop_asum_reverse = property $ do
  as <- forAll $ Gen.list (Range.singleton 10) (Gen.double (Range.constant (-1) 1))
  let
    (s, t) = runST $ do
      v <- V.unsafeFromList $(known 10) as
      v' <- V.reverse v
      (,) <$> dasum v <*> dasum v'
  annotateShow s
  annotateShow t
  assert $ equiv t s

mk_prop_interleave :: (forall n s. V s n Double -> ST s Double) -> Property
mk_prop_interleave f = property $ do
  _as <- forAll $ Gen.list (Range.singleton 5) (Gen.double (Range.constant (-1) 1))
  _bs <- forAll $ Gen.list (Range.singleton 5) (Gen.double (Range.constant (-1) 1))
  let
    a1 = runST $ f =<< V.fromList $(known 5) _as
    b1 = runST $ f =<< V.fromList $(known 5) _bs
    (a2, b2) = runST $ do
      _as <- V.fromList $(known 5) _as
      _bs <- V.fromList $(known 5) _bs
      (_as, _bs) <- interleave (_as, _bs)
      (,) <$> f _as <*> f _bs
  annotateShow (a1, a2)
  assert $ equiv a2 a1
  annotateShow (b1, b2)
  assert $ equiv b2 b1

prop_asum_interleave :: Property
prop_asum_interleave = mk_prop_interleave dasum

prop_iamax_known :: Property
prop_iamax_known = property $ do
  let
    true = [0, 0, 1, 1, 2]
    comp :: (forall s. ST s (Known s)) -> [CSize]
    comp dat = runST $ do
      Known {..} <- dat
      sequence
        [ idamax dv1
        , idamax dv2
        , idamax dv3
        , idamax dv4
        , idamax dv5
        ]
  comp knownData1 === true
  comp knownData2 === true

prop_scal_known :: Property
prop_scal_known = property $ do
  let
    scal1 = [0.3E0, -1.0E0, 0.0E0, 1.0E0, 0.3E0]
    scal2 = [0.3E0, 0.3E0, 0.3E0, 0.3E0, 0.3E0]
    true :: (forall s. ST s (Known s)) -> [[Double]]
    true dat = runST $ do
      Known {..} <- dat
      sequence
        [ V.toList dv1
        , V.toList dv2
        , V.toList dv3
        , V.toList dv4
        , V.toList dv5
        ]
    comp :: [Double] -> (forall s. ST s (Known s)) -> [[Double]]
    comp scals dat = runST $ do
      let [s1, s2, s3, s4, s5] = scals
      Known {..} <- dat
      dscal s1 dv1
      dscal s2 dv2
      dscal s3 dv3
      dscal s4 dv4
      dscal s5 dv5
      sequence
        [ V.toList dv1
        , V.toList dv2
        , V.toList dv3
        , V.toList dv4
        , V.toList dv5
        ]
  true knownScaled1 === comp scal1 knownData1
  true knownScaled2 === comp scal2 knownData2

main :: IO ()
main = void $ checkParallel $$(discover)
