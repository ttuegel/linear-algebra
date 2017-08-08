{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Presburger #-}

module Data.Vector.Blas
  ( V
  , length, null, slice, create
  , empty, singleton
  , replicate, generate, iterateN
  , replicateM, generateM, iterateNM
  , enumFromN, enumFromStepN
  , concat, copy, force
  , reverse
  , imap, map, imapM, mapM, mapM_, imapM_, forM_
  , zipWith, izipWith, zipWithM, izipWithM, zipWithM_, izipWithM_
  , zipWith3, izipWith3
  , foldl, foldl1, foldl', foldl1', ifoldl, ifoldl'
  , foldr, foldr1, foldr', foldr1', ifoldr, ifoldr'
  , foldlM, ifoldlM, foldlM', ifoldlM', foldl1M, foldl1M'
  , foldrM, ifoldrM, foldrM', ifoldrM', foldr1M, foldr1M'
  , all, any, and, or, sum, product
  , maximum, maximumBy, minimum, minimumBy
  , minIndex, minIndexBy, maxIndex, maxIndexBy
  , unsafeFromList, fromList, toList, litV
  ) where

import Control.Applicative hiding (empty)
import Control.Monad hiding (foldM, forM_, mapM, mapM_, replicateM, zipWithM, zipWithM_)
import Control.Monad.Base
import Control.Monad.ST.Strict
import Data.Bool
import Data.Eq
import Data.Function
import Data.Ord
import Language.Haskell.TH hiding (dyn)
import Prelude (Num(..), ($!), (++), error, fromIntegral, show)

import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.Foldable as Foldable
import qualified Language.Haskell.TH.Syntax as TH
import qualified Prelude

import Internal.Int
import Internal.Vector

length :: V s n a -> N n
length (V {..}) = vdim

null :: V s n a -> Bool
null (V {..}) = fromN vdim == 0

empty :: Storable a => ST s (V s 0 a)
empty = new $(known 0)

tail :: forall a n s. Storable a => V s (n + 1) a -> V s n a
tail as = slice $(known 1) n $(known 1) as where
  n :: N n
  n = N (fromN (length as) - 1)

init :: forall a n s. Storable a => V s (n + 1) a -> V s n a
init as = slice $(known 0) n $(known 1) as where
  n :: N n
  n = N (fromN (length as) - 1)

singleton :: Storable a => a -> ST s (V s 1 a)
singleton a =
  create $(known 1) $ \v ->
  write v (bounded $(known 0) $(known 0) $(known 1)) a

replicate :: Storable a => N n -> a -> ST s (V s n a)
replicate n a =
  create n $ \v -> Foldable.for_ (enumN n) (\i -> write v i a)

generate :: Storable a => N n -> (B 0 n -> a) -> ST s (V s n a)
generate n f =
  create n $ \v -> Foldable.for_ (enumN n) (\i -> write v i $! f i)

iterateN :: Storable a => N n -> (a -> a) -> a -> ST s (V s n a)
iterateN n f _a =
  create n $ \v ->
  Foldable.foldlM (\_a i -> write v i _a >> pure (f _a)) _a (enumN n)

create :: Storable a => N n -> (V s n a -> ST s b) -> ST s (V s n a)
create n go = new n >>= \v -> go v >> pure v

createM :: (MonadBase (ST s) m, Storable a) =>
           N n -> (V s n a -> m b) -> m (V s n a)
createM n go = liftBase (new n) >>= \v -> go v >> pure v

replicateM :: (MonadBase (ST s) m, Storable a) => N n -> m a -> m (V s n a)
replicateM n get =
  createM n $ \v ->
  Foldable.for_ (enumN n) (\i -> liftBase . write v i =<< get)

generateM :: (MonadBase (ST s) m, Storable a) =>
             N n -> (B 0 n -> m a) -> m (V s n a)
generateM n get =
  createM n $ \v ->
  Foldable.for_ (enumN n) (\i -> liftBase . write v i =<< get i)

iterateNM :: (MonadBase (ST s) m, Storable a) =>
             N n -> (a -> m a) -> a -> m (V s n a)
iterateNM n f _a =
  createM n $ \v ->
  Foldable.foldlM (\_a i -> liftBase (write v i _a) >> f _a) _a (enumN n)

enumFromN :: (Num a, Storable a) => a -> N n -> ST s (V s n a)
enumFromN from nn = enumFromStepN from 1 nn

enumFromStepN :: (Num a, Storable a) => a -> a -> N n -> ST s (V s n a)
enumFromStepN from step nn = iterateN nn (+ step) from

concat :: forall s m n a.
          Storable a =>
          V s m a -> V s n a -> ST s (V s (m + n) a)
concat as bs =
  let
    m = length as
    n = length bs
    mn = N (fromN m + fromN n) :: N (m + n)
  in
    create mn $ \v -> do
      copy (slice m n $(known 1) v) bs
      copy (slice $(known 0) m $(known 1) v) as

force :: Storable a => V s n a -> ST s (V s n a)
force as = create (length as) (\forced -> copy forced as)

reverse :: Storable a => V s (n + 1) a -> V s (n + 1) a
reverse as = ecils (minB ($(known 1), $(known 1))) (length as) $(known 1) as

imap :: (Storable a, Storable b) => (B 0 n -> a -> b) -> V s n a -> ST s (V s n b)
imap f as = generateM (length as) (\i -> f i <$> read as i)

map :: (Storable a, Storable b) => (a -> b) -> V s n a -> ST s (V s n b)
map f as = generateM (length as) (\i -> f <$> read as i)

mapM :: (MonadBase (ST s) m, Storable a, Storable b) =>
        (a -> m b) -> V s n a -> m (V s n b)
mapM f as = generateM (length as) (\i -> liftBase (read as i) >>= f)

imapM :: (MonadBase (ST s) m, Storable a, Storable b) =>
         (B 0 n -> a -> m b) -> V s n a -> m (V s n b)
imapM f as = generateM (length as) (\i -> liftBase (read as i) >>= f i)

mapM_ :: (MonadBase (ST s) m, Storable a) =>
         (a -> m b) -> V s n a -> m ()
mapM_ f as =
  Foldable.for_ (enumN (length as)) (\i -> liftBase (read as i) >>= f)

forM_ :: (MonadBase (ST s) m, Storable a) =>
         V s n a -> (a -> m b) -> m ()
forM_ as f = mapM_ f as

imapM_ :: (MonadBase (ST s) m, Storable a) =>
          (B 0 n -> a -> m b) -> V s n a -> m ()
imapM_ f as =
  Foldable.for_ (enumN (length as)) (\i -> liftBase (read as i) >>= f i)

zipWith :: (Storable a, Storable b, Storable c) =>
           (a -> b -> c) -> V s n a -> V s n b -> ST s (V s n c)
zipWith f as bs =
  generateM (length as) $ \i -> f <$> read as i <*> read bs i

izipWith :: (Storable a, Storable b, Storable c) =>
            (B 0 n -> a -> b -> c) -> V s n a -> V s n b -> ST s (V s n c)
izipWith f as bs =
  generateM (length as) $ \i -> f i <$> read as i <*> read bs i

zipWithM :: (MonadBase (ST s) m, Storable a, Storable b, Storable c) =>
            (a -> b -> m c) -> V s n a -> V s n b -> m (V s n c)
zipWithM f as bs =
  generateM (length as) $ \i -> do
    a <- liftBase $ read as i
    b <- liftBase $ read bs i
    f a b

izipWithM :: (MonadBase (ST s) m, Storable a, Storable b, Storable c) =>
             (B 0 n -> a -> b -> m c) -> V s n a -> V s n b
          -> m (V s n c)
izipWithM f as bs =
  generateM (length as) $ \i -> do
    a <- liftBase $ read as i
    b <- liftBase $ read bs i
    f i a b

zipWithM_ :: (MonadBase (ST s) m, Storable a, Storable b) =>
             (a -> b -> m c) -> V s n a -> V s n b -> m ()
zipWithM_ f as bs =
  Foldable.for_ (enumN (length as)) $ \i -> do
    a <- liftBase $ read as i
    b <- liftBase $ read bs i
    f a b

izipWithM_ :: (MonadBase (ST s) m, Storable a, Storable b) =>
              (B 0 n -> a -> b -> m c) -> V s n a -> V s n b -> m ()
izipWithM_ f as bs =
  Foldable.for_ (enumN (length as)) $ \i -> do
    a <- liftBase $ read as i
    b <- liftBase $ read bs i
    f i a b

zipWith3 :: (Storable a, Storable b, Storable c, Storable d) =>
            (a -> b -> c -> d) -> V s n a -> V s n b -> V s n c
         -> ST s (V s n d)
zipWith3 f as bs cs =
  generateM (length as) $ \i -> do
    a <- read as i
    b <- read bs i
    c <- read cs i
    pure $ f a b c

izipWith3 :: (Storable a, Storable b, Storable c, Storable d) =>
             (B 0 n -> a -> b -> c -> d) -> V s n a -> V s n b -> V s n c
          -> ST s (V s n d)
izipWith3 f as bs cs =
  generateM (length as) $ \i -> do
    a <- read as i
    b <- read bs i
    c <- read cs i
    pure $ f i a b c

foldl :: Storable b => (a -> b -> a) -> a -> V s n b -> ST s a
foldl f _a bs = Foldable.foldlM foldl_go _a (enumN (length bs)) where
  foldl_go _a !i = f _a <$> read bs i

foldl1 :: (Storable a) => (a -> a -> a) -> V s (n + 1) a -> ST s a
foldl1 f as = do
  a <- read as (minB (bounds as))
  foldl f a (tail as)

foldl' :: Storable b => (a -> b -> a) -> a -> V s n b -> ST s a
foldl' f !_a bs = Foldable.foldlM foldl'_go _a (enumN (length bs)) where
  foldl'_go !_a !i = f _a <$> read bs i

foldl1' :: (Storable a) => (a -> a -> a) -> V s (n + 1) a -> ST s a
foldl1' f as = do
  !a <- read as (minB (bounds as))
  foldl' f a (tail as)

foldr :: Storable a => (a -> b -> b) -> b -> V s n a -> ST s b
foldr f _b as = Foldable.foldrM foldr_go _b (enumN (length as)) where
  foldr_go !i _b = f <$> read as i <*> pure _b

foldr1 :: (Storable a) => (a -> a -> a) -> V s (n + 1) a -> ST s a
foldr1 f as = do
  a <- read as (maxB (bounds as))
  foldr f a (init as)

foldr' :: Storable a => (a -> b -> b) -> b -> V s n a -> ST s b
foldr' f !_b as = Foldable.foldrM foldr_go _b (enumN (length as)) where
  foldr_go !i !_b = f <$> read as i <*> pure _b

foldr1' :: (Storable a) => (a -> a -> a) -> V s (n + 1) a -> ST s a
foldr1' f as = do
  !a <- read as (maxB (bounds as))
  foldr' f a (init as)

ifoldl :: Storable b => (a -> B 0 n -> b -> a) -> a -> V s n b -> ST s a
ifoldl f _a bs = Foldable.foldlM ifoldl_go _a (enumN (length bs)) where
  ifoldl_go _a !i = f _a i <$> read bs i

ifoldl' :: Storable b => (a -> B 0 n -> b -> a) -> a -> V s n b -> ST s a
ifoldl' f !_a bs = Foldable.foldlM ifoldl'_go _a (enumN (length bs)) where
  ifoldl'_go !_a !i = f _a i <$> read bs i

ifoldr :: Storable a => (B 0 n -> a -> b -> b) -> b -> V s n a -> ST s b
ifoldr f _b as = Foldable.foldrM ifoldr_go _b (enumN (length as)) where
  ifoldr_go !i _b = f i <$> read as i <*> pure _b

ifoldr' :: Storable a => (B 0 n -> a -> b -> b) -> b -> V s n a -> ST s b
ifoldr' f !_b as = Foldable.foldrM ifoldr'_go _b (enumN (length as)) where
  ifoldr'_go !i !_b = f i <$> read as i <*> pure _b

all :: Storable a => (a -> Bool) -> V s n a -> ST s Bool
all pred = foldr (\a r -> pred a && r) True

any :: Storable a => (a -> Bool) -> V s n a -> ST s Bool
any pred = foldr (\a r -> pred a || r) False

and :: V s n Bool -> ST s Bool
and = foldr (&&) True

or :: V s n Bool -> ST s Bool
or = foldr (||) False

sum :: (Num a, Storable a) => V s n a -> ST s a
sum = foldl' (+) 0

product :: (Num a, Storable a) => V s n a -> ST s a
product = foldl' (*) 1

maximum :: (Ord a, Storable a) => V s (n + 1) a -> ST s a
maximum = foldl1' max

maximumBy :: (Storable a) => (a -> a -> Ordering) -> V s (n + 1) a -> ST s a
maximumBy comp = foldl1' maxBy where
  maxBy a b =
    case comp a b of
      LT -> b
      _ -> a

minimum :: (Ord a, Storable a) => V s (n + 1) a -> ST s a
minimum = foldl1' min

minimumBy :: (Storable a) => (a -> a -> Ordering) -> V s (n + 1) a -> ST s a
minimumBy comp = foldl1' minBy where
  minBy a b =
    case comp a b of
      GT -> b
      _ -> a

foldlM :: (MonadBase (ST s) m, Storable b) =>
          (a -> b -> m a) -> a -> V s n b -> m a
foldlM f _a bs = Foldable.foldlM foldlM_go _a (enumN (length bs)) where
  foldlM_go _a !i = f _a =<< liftBase (read bs i)

ifoldlM :: (MonadBase (ST s) m, Storable b) =>
           (a -> B 0 n -> b -> m a) -> a -> V s n b -> m a
ifoldlM f _a bs = Foldable.foldlM ifoldlM_go _a (enumN (length bs)) where
  ifoldlM_go _a !i = f _a i =<< liftBase (read bs i)

foldlM' :: (MonadBase (ST s) m, Storable b) =>
           (a -> b -> m a) -> a -> V s n b -> m a
foldlM' f _a bs = Foldable.foldlM foldlM'_go _a (enumN (length bs)) where
  foldlM'_go !_a !i = f _a =<< liftBase (read bs i)

ifoldlM' :: (MonadBase (ST s) m, Storable b) =>
            (a -> B 0 n -> b -> m a) -> a -> V s n b -> m a
ifoldlM' f _a bs = Foldable.foldlM ifoldlM'_go _a (enumN (length bs)) where
  ifoldlM'_go !_a !i = f _a i =<< liftBase (read bs i)

foldl1M :: (MonadBase (ST s) m, Storable a) =>
           (a -> a -> m a) -> V s (n + 1) a -> m a
foldl1M f as = do
  a <- liftBase $ read as (minB (bounds as))
  foldlM f a (tail as)

foldl1M' :: (MonadBase (ST s) m, Storable a) =>
            (a -> a -> m a) -> V s (n + 1) a -> m a
foldl1M' f as = do
  !a <- liftBase $ read as (minB (bounds as))
  foldlM' f a (tail as)

foldrM :: (MonadBase (ST s) m, Storable b) =>
          (b -> a -> m a) -> a -> V s n b -> m a
foldrM f _a bs = Foldable.foldrM foldrM_go _a (enumN (length bs)) where
  foldrM_go !i _a = do
    b <- liftBase (read bs i)
    f b _a

ifoldrM :: (MonadBase (ST s) m, Storable b) =>
           (B 0 n -> b -> a -> m a) -> a -> V s n b -> m a
ifoldrM f _a bs = Foldable.foldrM ifoldrM_go _a (enumN (length bs)) where
  ifoldrM_go !i _a = do
    b <- liftBase (read bs i)
    f i b _a

foldrM' :: (MonadBase (ST s) m, Storable b) =>
           (b -> a -> m a) -> a -> V s n b -> m a
foldrM' f _a bs = Foldable.foldrM foldrM'_go _a (enumN (length bs)) where
  foldrM'_go !i !_a = do
    b <- liftBase (read bs i)
    f b _a

ifoldrM' :: (MonadBase (ST s) m, Storable b) =>
            (B 0 n -> b -> a -> m a) -> a -> V s n b -> m a
ifoldrM' f _a bs = Foldable.foldrM ifoldrM'_go _a (enumN (length bs)) where
  ifoldrM'_go !i !_a = do
    b <- liftBase (read bs i)
    f i b _a

foldr1M :: (MonadBase (ST s) m, Storable a) =>
           (a -> a -> m a) -> V s (n + 1) a -> m a
foldr1M f as = do
  a <- liftBase $ read as (minB (bounds as))
  foldrM f a (tail as)

foldr1M' :: (MonadBase (ST s) m, Storable a) =>
            (a -> a -> m a) -> V s (n + 1) a -> m a
foldr1M' f as = do
  !a <- liftBase $ read as (minB (bounds as))
  foldrM' f a (tail as)

minIndex :: (Ord a, Storable a) => V s (n + 1) a -> ST s (B 0 (n + 1))
minIndex = minIndexBy compare

minIndexBy :: (Storable a) => (a -> a -> Ordering) -> V s (n + 1) a -> ST s (B 0 (n + 1))
minIndexBy comp as = do
  let zero = minB (bounds as)
  a <- read as zero
  State.execStateT (Foldable.foldlM minIndexBy_go a (enumN (length as))) zero
  where
    minIndexBy_go amin !i = do
      a <- liftBase (read as i)
      case comp a amin of
        LT -> State.put i >> pure a
        _ -> pure amin

maxIndex :: (Ord a, Storable a) => V s (n + 1) a -> ST s (B 0 (n + 1))
maxIndex = maxIndexBy compare

maxIndexBy :: (Storable a) => (a -> a -> Ordering) -> V s (n + 1) a -> ST s (B 0 (n + 1))
maxIndexBy comp as = do
  let zero = minB (bounds as)
  a <- read as zero
  State.execStateT (Foldable.foldlM maxIndexBy_go a (enumN (length as))) zero
  where
    maxIndexBy_go amax !i = do
      a <- liftBase (read as i)
      case comp a amax of
        GT -> State.put i >> pure a
        _ -> pure amax

unsafeFromList :: Storable a => N n -> [a] -> ST s (V s n a)
unsafeFromList n = State.evalStateT (replicateM n unsafeFromList_go) where
  unsafeFromList_go = State.get >>= \(a : as) -> State.put as >> pure a

fromList :: Storable a => N n -> [a] -> ST s (V s n a)
fromList n as
  | lenActual == lenTyped = unsafeFromList n as
  | otherwise =
      error ("Data.Vector.Blas.fromList:\nArgument has length " ++ show lenActual
              ++ ", but the result type requires length " ++ show lenTyped)

  where
    lenActual = Prelude.length as
    lenTyped = fromIntegral (fromN n)

toList :: Storable a => V s n a -> ST s [a]
toList = foldr (:) []

litV :: TH.Lift a => [a] -> Q Exp
litV as =
  let
    n = fromIntegral (Prelude.length as)
  in
    [| unsafeFromList $(known n) $(TH.lift as) |]
