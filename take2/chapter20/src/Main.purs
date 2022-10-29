module Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Data.Tuple (Tuple)
import Data.Identity (Identity)

newtype ReaderT r m a = ReaderT (r -> m a)
newtype StateT s m a = StateT (s -> m (Tuple a s))
newtype WriterT w m a = WriterT (m (Tuple a w))

type Reader r a = ReaderT r Identity a
type Writer w = WriterT w Identity
type State s = StateT s Identity

class MonadTrans t where
  lift :: forall m a. Monad m => m a -> t m a

type Nested r s w a = ReaderT r (StateT s (WriterT w Identity)) a

class Monad m <= MonadTell w m | m -> w where
  tell :: w -> m Unit

instance MonadTrans (ReaderT r) where
  lift :: forall m a. Monad m => m a -> ReaderT r m a
  lift = ReaderT <<< const

instance Monad m => Functor (ReaderT r m) where
  map f (ReaderT fx) = ReaderT \r -> do
    x <- fx r
    pure $ f x

instance Monad m => Apply (ReaderT r m) where
  apply (ReaderT ff) (ReaderT fx) = ReaderT \r -> do
    f <- ff r
    x <- fx r
    pure $ f x

instance Monad m => Bind (ReaderT r m) where
  bind (ReaderT fx) ff = ReaderT \r -> do
    x <- fx r
    let (ReaderT f) = ff x
    f r

instance Monad m => Applicative (ReaderT r m) where
  pure = ReaderT <<< const <<< pure

instance Monad m => Monad (ReaderT r m)

instance MonadTell w m => MonadTell w (ReaderT r m) where
  tell :: w -> ReaderT r m Unit
  tell = lift <<< tell

-- nested :: Nested { debug :: Boolean } { count :: Int } String Unit
-- nested = do
--   { debug } <- ask
--   { count } <- lift get
--   lift <<< lift $ tell $ "the count is: " <> show count
--   pure unit

main :: Effect Unit
main = do
  log "🍝"
