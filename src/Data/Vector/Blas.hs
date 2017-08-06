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
  , reverse
  , imap, map, imapM, mapM, mapM_, imapM_, forM_
  , zipWith, izipWith, zipWithM, izipWithM, zipWithM_, izipWithM_
  , zipWith3, izipWith3
  , foldl, foldl1, foldl', foldl1', ifoldl, ifoldl'
  , foldr, foldr1, foldr', foldr1', ifoldr, ifoldr'
  , foldM, ifoldM, foldM', ifoldM', fold1M, fold1M'
  , all, any, and, or, sum, product
  , maximum, maximumBy, minimum, minimumBy
  , minIndex, minIndexBy, maxIndex, maxIndexBy
  , unsafeFromList, fromList, litV, toDynV
  ) where

import Control.Applicative hiding (empty)
import Control.Monad hiding (foldM, forM_, mapM, mapM_, replicateM, zipWithM, zipWithM_)
import Control.Monad.Primitive
import Control.Monad.ST
import qualified Control.Monad.Trans.State.Strict as State
import Data.Bool
import Data.Eq
import Data.Function
import Data.Ord
import GHC.Prim (State#)
import Language.Haskell.TH hiding (dyn)
import Language.Haskell.TH.Syntax (Lift(..))
import Prelude (Num(..), ($!), (++), error, fromIntegral, fst, show)
import qualified Prelude

import Internal.Mut
import Internal.Int
import Internal.Vector hiding (slice, unsafeSlice)
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

slice :: (Storable a, i <= k, i + l <= k) => N i -> N l -> V k a -> V l a
slice ni nl as
  | lessThanOrEqual ni (length as) && lessThanOrEqual (plus ni nl) (length as) =
      unsafeSlice (toI ni) nl as
  | otherwise =
      error ("slice: index `" ++ show ni
              ++ "' out of bounds `" ++ show (length as) ++ "'")

unsafeSlice :: Storable a => I -> N l -> V k a -> V l a
unsafeSlice i nl (V _ fptr inc) =
  let fptr' = advanceForeignPtr fptr (fromIntegral $! i * inc) in
    V nl fptr' inc

empty :: Storable a => V 0 a
empty = create (new $(known 0))

singleton :: Storable a => a -> V 1 a
singleton a = create $ do
  mv <- new $(known 1)
  write mv $(bounded 0 1) a
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

replicateM :: forall a m n. (Monad m, Storable a) => N n -> m a -> m (V n a)
replicateM nn@(N n) get = runST (primitive replicateM_go) where
  replicateM_go :: forall s.
                   State# (PrimState (ST s))
                -> (# State# (PrimState (ST s)), m (V n a) #)
  replicateM_go _s =
    case internal (new nn :: ST s (Mut (V n) (PrimState (ST s)) a)) _s of
      (# _s, tmp #) ->
        let
          replicateM_loop :: State# (PrimState (ST s)) -> I -> m (V n a)
          replicateM_loop _s !i
              | i < 0 =
                  case internal (unsafeFreeze tmp :: ST s (V n a)) _s of
                    (# _, v #) -> pure v
              | otherwise = do
                  a <- get
                  case internal (unsafeWrite tmp i a :: ST s ()) _s of
                    (# _s, () #) -> replicateM_loop _s (i - 1)
        in (# _s, replicateM_loop _s (n - 1) #)


generateM :: forall a m n. (Monad m, Storable a) => N n -> (I -> m a) -> m (V n a)
generateM nn@(N n) get = runST (primitive generateM_go) where
  generateM_go :: forall s.
                  State# (PrimState (ST s))
               -> (# State# (PrimState (ST s)), m (V n a) #)
  generateM_go _s =
    case internal (new nn :: ST s (Mut (V n) (PrimState (ST s)) a)) _s of
      (# _s, tmp #) ->
        let
          generateM_loop :: State# (PrimState (ST s)) -> I -> m (V n a)
          generateM_loop _s !i
              | i < 0 =
                  case internal (unsafeFreeze tmp :: ST s (V n a)) _s of
                    (# _, v #) -> pure v
              | otherwise = do
                  a <- get i
                  case internal (unsafeWrite tmp i a :: ST s ()) _s of
                    (# _s, () #) -> generateM_loop _s (i - 1)
        in (# _s, generateM_loop _s (n - 1) #)

iterateNM :: forall a m n.
             (Monad m, Storable a) =>
             N n -> (a -> m a) -> a -> m (V n a)
iterateNM nn@(N n) get _a = runST (primitive iterateNM_go) where
  iterateNM_go :: forall s.
                  State# (PrimState (ST s))
               -> (# State# (PrimState (ST s)), m (V n a) #)
  iterateNM_go _s =
    case internal (new nn :: ST s (Mut (V n) (PrimState (ST s)) a)) _s of
      (# _s, tmp #) ->
        let
          iterateNM_loop :: State# (PrimState (ST s)) -> I -> a -> m (V n a)
          iterateNM_loop _s !i _a
              | i == n =
                  case internal (unsafeFreeze tmp :: ST s (V n a)) _s of
                    (# _, v #) -> pure v
              | otherwise = do
                  _a <- get _a
                  case internal (unsafeWrite tmp i _a :: ST s ()) _s of
                    (# _s, () #) -> iterateNM_loop _s (i + 1) _a
        in (# _s, iterateNM_loop _s 0 _a #)

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
  copy (Internal.unsafeSlice m nn v) bs
  copy (Internal.unsafeSlice 0 nm v) as
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

foldl :: forall a b n.
         Storable b => (a -> b -> a) -> a -> V n b -> a
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

foldl1 :: forall a n.
          (Storable a, 0 < n) =>
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

foldl' :: forall a b n. Storable b => (a -> b -> a) -> a -> V n b -> a
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

foldl1' :: forall a n.
           (Storable a, 0 < n) =>
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

foldr :: forall a b n. Storable a => (a -> b -> b) -> b -> V n a -> b
foldr f b as = foldr_loop 0 where
  N n = length as
  foldr_loop !i
    | i == n = b
    | otherwise = f (unsafeIndex as i) (foldr_loop (i + 1))

foldr1 :: forall a n.
          (Storable a, 0 < n) =>
          (a -> a -> a) -> V n a -> a
foldr1 f as = foldr1_loop 0 where
  N n = length as
  end = n - 1
  foldr1_loop !i
    | i == end = unsafeIndex as i
    | otherwise = f (unsafeIndex as i) (foldr1_loop (i + 1))

foldr' :: forall a b n. Storable a => (a -> b -> b) -> b -> V n a -> b
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

foldr1' :: forall a n.
           (Storable a, 0 < n) =>
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

ifoldl :: forall a b n.
          Storable b =>
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

ifoldl' :: forall a b n.
           Storable b =>
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

ifoldr :: forall a b n.
          Storable a =>
          (I -> a -> b -> b) -> b -> V n a -> b
ifoldr f b as = ifoldr_loop 0 where
  N n = length as
  ifoldr_loop !i
    | i == n = b
    | otherwise = f i (unsafeIndex as i) (ifoldr_loop (i + 1))

ifoldr' :: forall a b n.
           Storable a =>
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

maximum :: (Ord a, Storable a, 0 < n) => V n a -> a
maximum = foldl1' max

maximumBy :: (Storable a, 0 < n) => (a -> a -> Ordering) -> V n a -> a
maximumBy comp = foldl1' maxBy where
  maxBy a b =
    case comp a b of
      LT -> b
      _ -> a

minimum :: (Ord a, Storable a, 0 < n) => V n a -> a
minimum = foldl1' min

minimumBy :: (Storable a, 0 < n) => (a -> a -> Ordering) -> V n a -> a
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

fold1M :: (Monad m, Storable a, 0 < n) => (a -> a -> m a) -> V n a -> m a
fold1M f as = do
  let
    N n = length as
    fold1M_loop !i _a
      | i == n = pure _a
      | otherwise = f _a (unsafeIndex as i) >>= fold1M_loop (i + 1)
  fold1M_loop 1 (unsafeIndex as 0)

fold1M' :: (Monad m, Storable a, 0 < n) => (a -> a -> m a) -> V n a -> m a
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

minIndex :: (Ord a, Storable a, 0 < n) => V n a -> I
minIndex as =
  fst $ State.execState (imapM_ minIndex_go tail_as) (0, unsafeIndex as 0)
  where
    tail_as = slice $(known 1) (dyn $ toI (length as) - 1) as
    minIndex_go !i a = State.get >>= \(_, amin) ->
      when (a < amin) (State.put (i, a))

minIndexBy :: (Storable a, 0 < n) => (a -> a -> Ordering) -> V n a -> I
minIndexBy comp as =
  fst $ State.execState (imapM_ minIndexBy_go tail_as) (0, unsafeIndex as 0)
  where
    tail_as = slice $(sta 1) (dyn $ toI (length as) - 1) as
    minIndexBy_go !i a = do
      (_, amin) <- State.get
      case comp a amin of
        LT -> State.put (i, a)
        _ -> pure ()

maxIndex :: (Ord a, Storable a, 0 < n) => V n a -> I
maxIndex as =
  fst $ State.execState (imapM_ maxIndex_go tail_as) (0, unsafeIndex as 0)
  where
    tail_as = slice $(sta 1) (dyn $ toI (length as) - 1) as
    maxIndex_go !i a = State.get >>= \(_, amax) ->
      when (a > amax) (State.put (i, a))

maxIndexBy :: (Storable a, 0 < n) => (a -> a -> Ordering) -> V n a -> I
maxIndexBy comp as =
  fst $ State.execState (imapM_ maxIndexBy_go tail_as) (0, unsafeIndex as 0)
  where
    tail_as = slice $(sta 1) (dyn $ toI (length as) - 1) as
    maxIndexBy_go !i a = do
      (_, amax) <- State.get
      case comp a amax of
        GT -> State.put (i, a)
        _ -> pure ()

unsafeFromList :: Storable a => N n -> [a] -> V n a
unsafeFromList n = State.evalState (replicateM n unsafeFromList_go) where
  unsafeFromList_go = State.get >>= \(a : as) -> State.put as >> pure a

fromList :: forall a n. Storable a => [a] -> V n a
fromList as
  | lenActual == lenTyped = unsafeFromList n as
  | otherwise =
      error ("Data.Vector.Blas.fromList:\nArgument has length " ++ lenActual
              ++ ", but the result type requires length " ++ lenTyped)

  where
    lenActual = Prelude.length as
    lenTyped = fromIntegral (toI n)
    n :: N n
    n = N (fromIntegral (natVal (Proxy :: Proxy n)))

litV :: Lift a => [a] -> Q Exp
litV as =
  let
    n = fromIntegral (Prelude.length as)
  in
    [| unsafeFromList $(known n) $(lift as) |]
