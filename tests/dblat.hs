module Main where

import Hedgehog

import Data.Vector.Blas as V

testV :: V n a -> V n a -> V n a -> a -> PropertyT IO ()
testV comp true size sfac = do
  let f c t z =
        let sd = c - t in
          sdiff (abs z + abs (sfac * sd)) z
  assert $ (not . V.or) (V.zipWith3 f comp true size)
