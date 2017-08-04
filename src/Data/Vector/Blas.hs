{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Vector.Blas
  ( V
  , length, null, (!), slice, create
  , empty, singleton
  , replicate, generate, iterateN
  , replicateM, generateM, iterateNM
  , enumFromN, enumFromStepN
  , concat, copy, force
  ) where

import Control.Monad (when)
import Control.Monad.Primitive
import Control.Monad.ST
import Data.Proxy
import Language.Haskell.TH
import Prelude hiding (concat, foldl', foldr, length, mapM_, null, read, replicate)

import Internal.Mut
import Internal.Int
import Internal.Vector hiding (slice)
import qualified Internal.Vector as Internal

length :: V n a -> N n
length (V n _ _) = n

null :: V n a -> Bool
null (V (N n) _ _) = n == 0

(!) :: (Storable a, i < n) => V n a -> N i -> a
(!) as@(V nn _ _) ni@(N i)
  | lessThan ni nn = runST (unsafeIndexM as i)
  | otherwise =
      error ("Data.Vector.Blas.!: index `" ++ show ni
              ++ "' out of bounds `" ++ show nn ++ "'")

slice :: (Storable a, i < k, i + l <= k) => N i -> N l -> V k a -> V l a
slice ni@(N i) nl (V nn fptr inc)
  | lessThan ni nn && lessThanOrEqual (plus ni nl) nn =
      let fptr' = advanceForeignPtr fptr (fromIntegral $! i * inc) in
        V nl fptr' inc
  | otherwise =
      error ("slice: index `" ++ show ni
              ++ "' out of bounds `" ++ show nn ++ "'")

empty :: Storable a => V ('Sta 0) a
empty = create (new $(sta 0))

singleton :: Storable a => a -> V ('Sta 1) a
singleton a = create $ do
  mv <- new $(sta 1)
  write mv $(sta 0) a
  pure mv

replicate :: Storable a => N n -> a -> V n a
replicate nn@(N n) a = create $ do
  v <- new nn
  let
    replicate_go !i = do
      unsafeWrite v i a
      when (i /= 0) (replicate_go (i - 1))
  replicate_go (n - 1)
  pure v

create :: (forall s. ST s (Mut (V n) s a)) -> V n a
create go = runST (go >>= \(Mut v) -> pure v)

generate :: Storable a => N n -> (I -> a) -> V n a
generate nn@(N n)  f = create $ do
  v <- new nn
  let
    generate_go !i = do
      unsafeWrite v i $! f i
      when (i /= 0) (generate_go (i - 1))
  when (n > 0) (generate_go (n - 1))
  pure v

iterateN :: Storable a => N n -> (a -> a) -> a -> V n a
iterateN nn@(N n) f _a = create $ do
  v <- new nn
  let
    iterateN_go !i !_a
      | i == n = pure ()
      | otherwise = do
          unsafeWrite v i _a
          iterateN_go (i + 1) (f _a)
  iterateN_go 0 _a
  pure v

replicateM :: (Monad m, Storable a) => N n -> m a -> m (V n a)
replicateM nn@(N n) get = do
  v <- new nn
  let
    replicateM_go !i = do
      get >>= unsafeWrite v i
      when (i /= 0) (replicateM_go (i - 1))
  when (n > 0) (replicateM_go (n - 1))
  freeze v


generateM :: (Monad m, Storable a) => N n -> (I -> m a) -> m (V n a)
generateM nn@(N n) get = do
  v <- new nn
  let
    replicateM_go !i = do
      get i >>= unsafeWrite v i
      when (i /= 0) (replicateM_go (i - 1))
  when (n > 0) (replicateM_go (n - 1))
  freeze v

iterateNM :: (Monad m, Storable a) =>
             N n -> (a -> m a) -> a -> m (V n a)
iterateNM nn@(N n) get _a = do
  v <- new nn
  let
    iterateN_go !i !_a
      | i == n = pure ()
      | otherwise = do
          unsafeWrite v i _a
          iterateN_go (i + 1) =<< get _a
  iterateN_go 0 _a
  freeze v

enumFromN :: (Num a, Storable a) => a -> N n -> V n a
enumFromN from nn = enumFromStepN from 1 nn

enumFromStepN :: (Num a, Storable a) => a -> a -> N n -> V n a
enumFromStepN from step nn = iterateN nn (+ step) from

concat :: Storable a => V m a -> V n a -> V (m + n) a
concat as bs = create $ do
  let
    nm@(N m) = length as
    nn = length bs
  v <- new (plus nm nn)
  copy (unsafeSlice m nn v) bs
  copy (unsafeSlice 0 nm v) as
  pure v

force :: Storable a => V n a -> V n a
force as = create $ do
  forced <- new (length as)
  copy forced as
  pure forced

reverse :: Storable a => V n a -> V n a
reverse as =
  let
    nn@(N n) = length as
  in
    runST (generateM nn $ \i -> unsafeIndexM as (n - i))

imap :: (Storable a, Storable b) => (I -> a -> b) -> V n a -> V n b
imap f as =
  runST (generateM (length as) $ \i -> f i <$> unsafeIndexM as i)

map :: (Storable a, Storable b) => (a -> b) -> V n a -> V n b
map f as =
  runST (generateM (length as) $ \i -> f <$> unsafeIndexM as i)

mapM :: (Monad m, Storable a, Storable b) => (a -> m b) -> V n a -> m (V n b)
mapM f as = generateM (length as) $ \i -> f (unsafeIndex as i)

imapM :: (Monad m, Storable a, Storable b) =>
         (I -> a -> m b) -> V n a -> m (V n b)
imapM f as = generateM (length as) $ \i -> f i (unsafeIndex as i)

mapM_ :: (Monad m, Storable a) => (a -> m b) -> V n a -> m ()
mapM_ f as = do
  let
    N n = length as
    mapM_go !i
      | i == n = pure ()
      | otherwise = f (unsafeIndex as i) >> mapM_go (i + 1)
  mapM_go 0

forM_ :: (Monad m, Storable a) => V n a -> (a -> m b) -> m ()
forM_ as f = mapM_ f as

imapM_ :: (Monad m, Storable a) => (I -> a -> m b) -> V n a -> m ()
imapM_ f as = do
  let
    N n = length as
    imapM_go !i
      | i == n = pure ()
      | otherwise = f i (unsafeIndex as i) >> imapM_go (i + 1)
  imapM_go 0

zipWith :: (Storable a, Storable b, Storable c) =>
           (a -> b -> c) -> V n a -> V n b -> V n c
zipWith f as bs =
  runST $ generateM (length as) $ \i ->
    f <$> unsafeIndexPrim as i <*> unsafeIndexPrim bs i

izipWith :: (Storable a, Storable b, Storable c) =>
            (I -> a -> b -> c) -> V n a -> V n b -> V n c
izipWith f as bs =
  runST $ generateM (length as) $ \i ->
    f i <$> unsafeIndexPrim as i <*> unsafeIndexPrim bs i

zipWithM :: (PrimMonad m, Storable a, Storable b, Storable c) =>
            (a -> b -> m c) -> V n a -> V n b -> m (V n c)
zipWithM f as bs =
  generateM (length as) $ \i -> do
    a <- unsafeIndexM as i
    b <- unsafeIndexM bs i
    f a b

izipWithM :: (PrimMonad m, Storable a, Storable b, Storable c) =>
             (I -> a -> b -> m c) -> V n a -> V n b -> m (V n c)
izipWithM f as bs =
  generateM (length as) $ \i -> do
    a <- unsafeIndexM as i
    b <- unsafeIndexM bs i
    f i a b

zipWithM_ :: (PrimMonad m, Storable a, Storable b) =>
             (a -> b -> m c) -> V n a -> V n b -> m ()
zipWithM_ f as bs =
  forM_ (enumFromN 0 (length as)) $ \i -> do
    a <- unsafeIndexM as i
    b <- unsafeIndexM bs i
    f a b

izipWithM_ :: (PrimMonad m, Storable a, Storable b) =>
              (I -> a -> b -> m c) -> V n a -> V n b -> m ()
izipWithM_ f as bs =
  forM_ (enumFromN 0 (length as)) $ \i -> do
    a <- unsafeIndexM as i
    b <- unsafeIndexM bs i
    f i a b

zipWith3 :: (Storable a, Storable b, Storable c, Storable d) =>
            (a -> b -> c -> d) -> V n a -> V n b -> V n c -> V n d
zipWith3 f as bs cs =
  runST (generateM (length as) $ \i -> do
            a <- unsafeIndexM as i
            b <- unsafeIndexM bs i
            c <- unsafeIndexM cs i
            pure $ f a b c)

izipWith3 :: (Storable a, Storable b, Storable c, Storable d) =>
             (I -> a -> b -> c -> d) -> V n a -> V n b -> V n c -> V n d
izipWith3 f as bs cs =
  runST (generateM (length as) $ \i -> do
            a <- unsafeIndexM as i
            b <- unsafeIndexM bs i
            c <- unsafeIndexM cs i
            pure $ f i a b c)

foldl :: forall a b (n :: Dim). Storable b => (a -> b -> a) -> a -> V n b -> a
foldl f _a bs = runST foldl_go where
  foldl_go :: forall s. ST s a
  foldl_go = do
    let
      N n = length bs
      foldl_loop !i _a
        | i == n = pure _a
        | otherwise = do
            _a <- f _a <$> unsafeIndexM bs i
            foldl_loop (i + 1) _a
    foldl_loop 0 _a

foldl1 :: forall a (n :: Dim). (Storable a, 'Sta 0 < n) =>
          (a -> a -> a) -> V n a -> a
foldl1 f as = runST foldl1_go where
  foldl1_go :: forall s. ST s a
  foldl1_go = do
    let
      N n = length as
      foldl1_loop !i _a
        | i == n = pure _a
        | otherwise = do
            _a <- f _a <$> unsafeIndexM as i
            foldl1_loop (i + 1) _a
    foldl1_loop 1 =<< unsafeIndexM as 0

foldl' :: forall a b (n :: Dim). Storable b => (a -> b -> a) -> a -> V n b -> a
foldl' f !_a bs = runST foldl'_go where
  foldl'_go :: forall s. ST s a
  foldl'_go = do
    let
      N n = length bs
      foldl'_loop !i !_a
        | i == n = pure _a
        | otherwise = do
            _a <- f _a <$> unsafeIndexM bs i
            foldl'_loop (i + 1) _a
    foldl'_loop 0 _a

foldl1' :: forall a (n :: Dim). (Storable a, 'Sta 0 < n) =>
           (a -> a -> a) -> V n a -> a
foldl1' f as = runST foldl1'_go where
  foldl1'_go :: forall s. ST s a
  foldl1'_go = do
    let
      N n = length as
      foldl1'_loop !i !_a
        | i == n = pure _a
        | otherwise = do
            _a <- f _a <$> unsafeIndexM as i
            foldl1'_loop (i + 1) _a
    foldl1'_loop 1 =<< unsafeIndexM as 0

foldr :: forall a b (n :: Dim). Storable a => (a -> b -> b) -> b -> V n a -> b
foldr f b as = foldr_loop 0 where
  N n = length as
  foldr_loop !i
    | i == n = b
    | otherwise = f (unsafeIndex as i) (foldr_loop (i + 1))

foldr1 :: forall a b (n :: Dim). (Storable a, 'Sta 0 < n) =>
          (a -> a -> a) -> V n a -> a
foldr1 f as = foldr1_loop 0 where
  N n = length as
  end = n - 1
  foldr1_loop !i
    | i == end = unsafeIndex as i
    | otherwise = f (unsafeIndex as i) (foldr1_loop (i + 1))

foldr' :: forall a b (n :: Dim). Storable a => (a -> b -> b) -> b -> V n a -> b
foldr' f !_b as = runST foldr'_go where
  foldr'_go :: forall s. ST s b
  foldr'_go = do
    let
      N n = length as
      foldr'_loop !i !_b
        | i < 0 = pure _b
        | otherwise = do
            !_b <- f <$> unsafeIndexM as i <*> pure _b
            foldr'_loop (i - 1) _b
    foldr'_loop (n - 1) _b

foldr1' :: forall a b (n :: Dim). (Storable a, 'Sta 0 < n) =>
           (a -> a -> a) -> V n a -> a
foldr1' f as = runST foldr1'_go where
  foldr1'_go :: forall s. ST s a
  foldr1'_go = do
    let
      N n = length as
      foldr1'_loop !i !_a
        | i < 0 = pure _a
        | otherwise = do
            !_a <- f <$> unsafeIndexM as i <*> pure _a
            foldr1'_loop (i - 1) _a
    !_a <- unsafeIndexM as (n - 1)
    foldr1'_loop (n - 2) _a

ifoldl :: forall a b (n :: Dim). Storable b =>
          (a -> I -> b -> a) -> a -> V n b -> a
ifoldl f _a bs = runST ifoldl_go where
  ifoldl_go :: forall s. ST s a
  ifoldl_go = do
    let
      N n = length bs
      ifoldl_loop !i _a
        | i == n = pure _a
        | otherwise = do
            _a <- f _a i <$> unsafeIndexM bs i
            ifoldl_loop (i + 1) _a
    ifoldl_loop 0 _a

ifoldl' :: forall a b (n :: Dim). Storable b =>
           (a -> I -> b -> a) -> a -> V n b -> a
ifoldl' f _a bs = runST ifoldl'_go where
  ifoldl'_go :: forall s. ST s a
  ifoldl'_go = do
    let
      N n = length bs
      ifoldl'_loop !i !_a
        | i == n = pure _a
        | otherwise = do
            !_a <- f _a i <$> unsafeIndexM bs i
            ifoldl'_loop (i + 1) _a
    ifoldl'_loop 0 _a

ifoldr :: forall a b (n :: Dim). Storable a =>
          (I -> a -> b -> b) -> b -> V n a -> b
ifoldr f b as = ifoldr_loop 0 where
  N n = length as
  ifoldr_loop !i
    | i == n = b
    | otherwise = f i (unsafeIndex as i) (ifoldr_loop (i + 1))

ifoldr' :: forall a b (n :: Dim). Storable a =>
           (I -> a -> b -> b) -> b -> V n a -> b
ifoldr' f !_b as = runST ifoldr'_go where
  ifoldr'_go :: forall s. ST s b
  ifoldr'_go = do
    let
      N n = length as
      ifoldr'_loop !i !_b
        | i < 0 = pure _b
        | otherwise = do
            !_b <- f i <$> unsafeIndexM as i <*> pure _b
            ifoldr'_loop (i - 1) _b
    ifoldr'_loop (n - 1) _b

all :: Storable a => (a -> Bool) -> V n a -> Bool
all pred = foldr (\a r -> pred a && r) True

any :: Storable a => (a -> Bool) -> V n a -> Bool
any pred = foldr (\a r -> pred a || r) False

and :: V n Bool -> Bool
and = foldr (&&) True

or :: V n Bool -> Bool
or = foldr (||) False

sum :: (Num a, Storable a) => V n a -> a
sum = foldl' (+) 0

product :: (Num a, Storable a) => V n a -> a
product = foldl' (*) 1

maximum :: ('Sta 0 < n, Ord a, Storable a) => V n a -> a
maximum = foldl1' max

maximumBy :: ('Sta 0 < n, Storable a) => (a -> a -> Ordering) -> V n a -> a
maximumBy comp = foldl1' maxBy where
  maxBy a b =
    case comp a b of
      LT -> b
      _ -> a

minimum :: ('Sta 0 < n, Ord a, Storable a) => V n a -> a
minimum = foldl1' min

minimumBy :: (Storable a, 'Sta 0 < n) => (a -> a -> Ordering) -> V n a -> a
minimumBy comp = foldl1' minBy where
  minBy a b =
    case comp a b of
      GT -> b
      _ -> a

foldM :: (Monad m, Storable b) => (a -> b -> m a) -> a -> V n b -> m a
foldM f _a bs = do
  let
    N n = length bs
    foldM_loop !i _a
      | i == n = pure _a
      | otherwise = f _a (unsafeIndex bs i) >>= foldM_loop (i + 1)
  foldM_loop 0 _a

ifoldM :: (Monad m, Storable b) => (a -> I -> b -> m a) -> a -> V n b -> m a
ifoldM f _a bs = do
  let
    N n = length bs
    ifoldM_loop !i _a
      | i == n = pure _a
      | otherwise = f _a i (unsafeIndex bs i) >>= ifoldM_loop (i + 1)
  ifoldM_loop 0 _a

foldM' :: (Monad m, Storable b) => (a -> b -> m a) -> a -> V n b -> m a
foldM' f _a bs = do
  let
    N n = length bs
    foldM'_loop !i !_a
      | i == n = pure _a
      | otherwise = do
          !_a <- f _a (unsafeIndex bs i)
          foldM'_loop (i + 1) _a
  foldM'_loop 0 _a

ifoldM' :: (Monad m, Storable b) => (a -> I -> b -> m a) -> a -> V n b -> m a
ifoldM' f _a bs = do
  let
    N n = length bs
    ifoldM'_loop !i !_a
      | i == n = pure _a
      | otherwise = do
          !_a <- f _a i (unsafeIndex bs i)
          ifoldM'_loop (i + 1) _a
  ifoldM'_loop 0 _a

fold1M :: ('Sta 0 < n, Monad m, Storable a) => (a -> a -> m a) -> V n a -> m a
fold1M f as = do
  let
    N n = length as
    fold1M_loop !i _a
      | i == n = pure _a
      | otherwise = f _a (unsafeIndex as i) >>= fold1M_loop (i + 1)
  fold1M_loop 1 (unsafeIndex as 0)

fold1M' :: ('Sta 0 < n, Monad m, Storable a) => (a -> a -> m a) -> V n a -> m a
fold1M' f as = do
  let
    N n = length as
    fold1M'_loop !i !_a
      | i == n = pure _a
      | otherwise = do
          !_a <- f _a (unsafeIndex as i)
          fold1M'_loop (i + 1) _a
    !_a = unsafeIndex as 0
  fold1M'_loop 1 _a

{-
backpermute :: V n a -> V n I -> V n a

modify :: (forall s. Mut (V n) s a -> ST s ()) -> V n a -> V n a

minIndex :: ('Sta 0 < n, Ord a) => V n a -> I

minIndexBy :: ('Sta 0 < n) => (a -> a -> Ordering) -> V n a -> I

maxIndex :: ('Sta 0 < n, Ord a) => V n a -> I

maxIndexBy :: ('Sta 0 < n) => (a -> a -> Ordering) -> V n a -> I

prescanl :: (a -> b -> a) -> a -> V n b -> V n a

prescanl' :: (a -> b -> a) -> a -> V n b -> V n a

postscanl :: (a -> b -> a) -> a -> V n b -> V n a

postscanl' :: (a -> b -> a) -> a -> V n b -> V n a

scanl :: (a -> b -> a) -> a -> V n b -> V (n + 'Sta 1) a

scanl' :: (a -> b -> a) -> a -> V n b -> V (n + 'Sta 1) a

scanl1 :: (a -> a -> a) -> V n a -> V n a

scanl1' :: (a -> a -> a) -> V n a -> V n a

iscanl :: (I -> a -> b -> a) -> a -> V n b -> V (n + 'Sta 1) a

iscanl' :: (I -> a -> b -> a) -> a -> V n b -> V (n + 'Sta 1) a

prescanr :: (a -> b -> b) -> b -> V n a -> V n b

prescanr' :: (a -> b -> b) -> b -> V n a -> V n b

postscanr :: (a -> b -> b) -> b -> V n a -> V n b

postscanr' :: (a -> b -> b) -> b -> V n a -> V n b

scanr :: (a -> b -> b) -> b -> V n a -> V (n + 'Sta 1) b

scanr' :: (a -> b -> b) -> b -> V n a -> V (n + 'Sta 1) b

scanr1 :: (a -> a -> a) -> V n a -> V n a

scanr1' :: (a -> a -> a) -> V n a -> V n a

iscanr :: (I -> a -> b -> b) -> b -> V n a -> V (n + 'Sta 1) b

iscanr' :: (I -> a -> b -> b) -> b -> V n a -> V (n + 'Sta 1) b

fromList :: [a] -> V 'Dyn a
fromList = unsafeFromList (Proxy :: Proxy 'Dyn)

unsafeFromList :: Proxy n -> [a] -> V n a
unsafeFromList _ as = _

unsafeSizeCast :: V j a -> V k a
unsafeSizeCast (V n p i) = V n p i

litV :: forall a. [a] -> Q Exp
litV as =
  let
    n = Prelude.length as
    dim = [t| 'Sta $(litT (numTyLit n)) |]
  in
    [| unsafeFromList (Proxy :: Proxy $dim) $(lift as) |]

-}
