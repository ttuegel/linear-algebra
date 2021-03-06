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
equiv comp true = sz + abs (sfac * (comp - true)) - sz == 0
  where
    sfac = 9.765625E-4
    sz = abs true

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

prop_scal_identity :: Property
prop_scal_identity = property $ do
  as <- forAll $ Gen.list (Range.singleton 10) (Gen.double (Range.constant (-1) 1))
  let
    as' = runST $ do
      _as <- V.fromList $(known 10) as
      dscal 1.0 _as
      V.toList _as
  as === as'

prop_scal_null :: Property
prop_scal_null = property $ do
  as <- forAll $ Gen.list (Range.singleton 10) (Gen.double (Range.constant (-1) 1))
  let
    as' = runST $ do
      _as <- V.fromList $(known 10) as
      dscal 0.0 _as
      V.toList _as
  map (const 0.0) as === as'

prop_scal_commute :: Property
prop_scal_commute = property $ do
  a <- forAll $ Gen.double (Range.constant (-1) 1)
  b <- forAll $ Gen.double (Range.constant (-1) 1)
  as <- forAll $ Gen.list (Range.singleton 10) (Gen.double (Range.constant (-1) 1))
  let
    (ab, ba) = runST $ do
      _as <- V.fromList $(known 10) as
      _bs <- V.fromList $(known 10) as
      dscal a _as
      dscal b _as
      dscal b _bs
      dscal a _bs
      (,) <$> V.toList _as <*> V.toList _bs
  annotateShow ab
  annotateShow ba
  assert $ and $ zipWith equiv ab ba

prop_scal_singleton :: Property
prop_scal_singleton = property $ do
  a <- forAll $ Gen.double (Range.constant (-1) 1)
  b <- forAll $ Gen.double (Range.constant (-1) 1)
  let
    ab = runST $ do
      v <- V.singleton a
      dscal b v
      V.read v (bounded $(known 0) $(known 0) $(known 1))
  a * b === ab

prop_copy :: Property
prop_copy = property $ do
  as <- forAll $ Gen.list (Range.singleton 10) (Gen.double (Range.constant (-1) 1))
  let
    as' = runST $ do
      _as <- V.fromList $(known 10) as
      _bs <- V.new $(known 10)
      V.copy _bs _as
      V.toList _bs
  as === as'

prop_swap :: Property
prop_swap = property $ do
  as <- forAll $ Gen.list (Range.singleton 10) (Gen.double (Range.constant (-1) 1))
  bs <- forAll $ Gen.list (Range.singleton 10) (Gen.double (Range.constant (-1) 1))
  let
    (as', bs') = runST $ do
      _as <- V.fromList $(known 10) as
      _bs <- V.fromList $(known 10) bs
      swap _as _bs
      (,) <$> V.toList _as <*> V.toList _bs
  as === bs'
  bs === as'

prop_axpy_commute :: Property
prop_axpy_commute = property $ do
  a <- forAll $ Gen.double (Range.constant (-1) 1)
  as <- forAll $ Gen.list (Range.singleton 10) (Gen.double (Range.constant (-1) 1))
  b <- forAll $ Gen.double (Range.constant (-1) 1)
  bs <- forAll $ Gen.list (Range.singleton 10) (Gen.double (Range.constant (-1) 1))
  cs <- forAll $ Gen.list (Range.singleton 10) (Gen.double (Range.constant (-1) 1))
  let
    (ab, ba) = runST $ do
      _as <- V.fromList $(known 10) as
      _bs <- V.fromList $(known 10) bs
      _ab <- V.fromList $(known 10) cs
      axpy a _as _ab
      axpy b _bs _ab
      _ba <- V.fromList $(known 10) cs
      axpy b _bs _ba
      axpy a _as _ba
      (,) <$> V.toList _ab <*> V.toList _ba
  annotateShow ab
  annotateShow ba
  assert $ and $ zipWith equiv ab ba

prop_axpy_identity :: Property
prop_axpy_identity = property $ do
  as <- forAll $ Gen.list (Range.singleton 10) (Gen.double (Range.constant (-1) 1))
  bs <- forAll $ Gen.list (Range.singleton 10) (Gen.double (Range.constant (-1) 1))
  let
    as' = runST $ do
      _as <- V.fromList $(known 10) as
      _bs <- V.fromList $(known 10) bs
      axpy 0.0 _bs _as
      V.toList _as
  as === as'

prop_axpy_null :: Property
prop_axpy_null = property $ do
  as <- forAll $ Gen.list (Range.singleton 10) (Gen.double (Range.constant (-1) 1))
  let
    as' = runST $ do
      _as <- V.fromList $(known 10) as
      _bs <- V.fromList $(known 10) as
      axpy (-1) _bs _as
      V.toList _as
  map (const 0.0) as === as'

main :: IO ()
main = void $ checkParallel $$(discover)
