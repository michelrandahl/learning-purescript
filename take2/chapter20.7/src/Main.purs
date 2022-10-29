module Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Data.Tuple (Tuple(..))

-- newtype Writer w a = Writer (Tuple a w)
newtype WriterT w m a = WriterT (m (Tuple a w))

runWriterT :: forall w m a. WriterT w m a -> m (Tuple a w)
runWriterT (WriterT mx) = mx

instance Functor m => Functor (WriterT w m) where
  map f (WriterT mx) =
    mx
    # map (\(Tuple x w) -> Tuple (f x) w)
    # WriterT

instance (Semigroup w, Monad m) => Apply (WriterT w m) where
  apply (WriterT mf) (WriterT mx) = WriterT do
    Tuple f w1 <- mf
    Tuple x w2 <- mx
    pure $ Tuple (f x) $ w1 <> w2

instance (Monoid w, Monad m) => Applicative (WriterT w m) where
  pure = WriterT <<< pure <<< flip Tuple mempty

instance (Semigroup w, Monad m) => Bind (WriterT w m) where
  bind (WriterT mx) f = WriterT do
    Tuple x w1 <- mx
    Tuple y w2 <- runWriterT $ f x
    pure $ Tuple y $ w1 <> w2

instance (Monoid w, Monad m) => Monad (WriterT w m)

--newtype Reader r a = Reader (r -> a)
newtype ReaderT r m a = ReaderT (r -> m a)

runReaderT :: forall r m a. ReaderT r m a -> (r -> m a)
runReaderT (ReaderT mf) = mf

instance Functor m => Functor (ReaderT r m) where
  map :: forall a b. (a -> b) -> (ReaderT r m) a -> (ReaderT r m) b
  map f (ReaderT mg) = ReaderT \r -> map f (mg r)

instance Apply m => Apply (ReaderT r m) where
  apply (ReaderT fmf) (ReaderT fmx) = ReaderT \r -> apply (fmf r) (fmx r)

class MonadTrans t where
  lift :: forall m a. Monad m => m a -> t m a

instance MonadTrans (ReaderT r) where
  lift = ReaderT <<< const

instance Monad m => Applicative (ReaderT r m) where
  pure = lift <<< pure

instance Bind m => Bind (ReaderT r m) where
  bind (ReaderT fmx) f = ReaderT \r -> do
    x <- fmx r
    runReaderT (f x) r

instance Monad m => Monad (ReaderT r m)

class Monad m <= MonadAsk r m | m -> r where
  ask :: m r

class Monad m <= MonadTell w m | m -> w where
  tell :: w -> m Unit

class Monad m <= MonadState s m | m -> s where
  state :: forall a. (s -> Tuple a s) -> m a

instance Monad m => MonadAsk r (ReaderT r m) where
  ask :: (ReaderT r m) r
  ask = ReaderT pure

instance MonadTell w m => MonadTell w (ReaderT r m) where
  tell = lift <<< tell

instance MonadState s m => MonadState s (ReaderT r m) where
  state = lift <<< state

-- the monad stack is defined by the nesting of the transformer types
-- app :: forall s r. StateT s (WriterT String (ReaderT r Identity)) Unit
-- app = do
--   tell ""
--   pure unit

main :: Effect Unit
main = do
  log "🍝"
