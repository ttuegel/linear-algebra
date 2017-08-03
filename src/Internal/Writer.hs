module Internal.Writer where

import Control.Monad.Trans.Class
import Data.Semigroup (Semigroup(..))


newtype WriterT w m a = WriterT { unWriterT :: w -> m (a, w) }

runWriterT :: (Monad m, Monoid w) => WriterT w m a -> m (a, w)
runWriterT wt = unWriterT wt mempty

instance Functor m => Functor (WriterT w m) where
  fmap f wr = WriterT $ \_w -> fmap (\(a, _w) -> (f a, _w)) (unWriterT wr _w)

instance Monad m => Applicative (WriterT w m) where
  pure a = WriterT $ \w -> pure (a, w)
  (<*>) wf wa  = WriterT $ \_w -> do
    (f, _w) <- unWriterT wf _w
    (a, _w) <- unWriterT wa _w
    pure (f a, _w)

instance Monad m => Monad (WriterT w m) where
  (>>=) wa kab = WriterT $ \_w -> do
    (a, _w) <- unWriterT wa _w
    unWriterT (kab a) _w

instance MonadTrans (WriterT w) where
  lift ma = WriterT $ \w -> fmap (\a -> (a, w)) ma

writerT :: (Monad m, Semigroup w) => m (a, w) -> WriterT w m a
writerT maw = WriterT $ \w0 -> do
  (a, w1) <- maw
  let w2 = w0 <> w1
  w2 `seq` pure (a, w2)

writer :: (Monad m, Semigroup w) => (a, w) -> WriterT w m a
writer aw = writerT (pure aw)

tell :: (Monad m, Semigroup w) => w -> WriterT w m ()
tell w = writer ((), w)
