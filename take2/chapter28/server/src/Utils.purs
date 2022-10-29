module Utils where

import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Aff.AVar (AVar)
import Data.Tuple (Tuple(..))
import Effect.Aff.AVar as AVar
import Prelude
import Data.Either (Either)
import Control.Monad.Trans.Class (lift, class MonadTrans)
import Control.Monad.Error.Class (class MonadThrow, liftEither)

withAVar :: forall a b m. MonadAff m => AVar a -> (a -> m (Tuple a b)) -> m b
withAVar avar f = do
  x <- liftAff $ AVar.take avar
  Tuple newx result <- f x
  liftAff $ AVar.put newx avar
  pure result

liftSuccess :: forall e a m t. MonadThrow e (t m) => MonadTrans t => Monad m => m (Either e a) -> t m a
liftSuccess ma = lift ma >>= liftEither
