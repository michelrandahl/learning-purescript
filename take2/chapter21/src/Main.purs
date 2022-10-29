module Main where

import Prelude

import Effect (Effect)
import Effect.Console as Console
import Data.Tuple (Tuple(..))
--import Data.Typelevel.Undefined (undefined)
import Control.Monad.State.Class (class MonadState, get, put)
import Control.Monad.Reader.Class (class MonadAsk, ask)
import Control.Monad.Writer.Class (class MonadTell, tell)
import Control.Monad.Trans.Class (class MonadTrans, lift)
import Control.Monad.Except.Trans (ExceptT, runExceptT)
import Control.Monad.Writer.Trans (WriterT, runWriterT)
import Control.Monad.Error.Class (class MonadThrow, class MonadError, throwError, catchError)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Debug (spy)

newtype State s a = State (s -> Tuple a s)

newtype StateT s m a = StateT (s -> m (Tuple a s))

runStateT :: forall s m a. StateT s m a -> s -> m (Tuple a s)
runStateT (StateT f) s = f s

instance Functor m => Functor (StateT s m) where
  --map f (StateT mg) = StateT \s -> mg s # map (\(Tuple a s2) -> Tuple (f a) s2) 
  map f (StateT mg) = StateT \s -> mg s <#> \(Tuple a s2) -> Tuple (f a) s2 

instance Monad m => Apply (StateT s m) where
  apply (StateT fmf) (StateT fmx) = StateT \s -> do
    Tuple f s2 <- fmf s
    Tuple a s3  <- fmx s2
    pure $ Tuple (f a) s3

instance Monad m => Applicative (StateT s m) where
  pure x = StateT \s -> pure $ Tuple x s

instance Monad m => Bind (StateT s m) where
  bind (StateT fmx) f = StateT \s ->
    fmx s >>= \(Tuple x s2) -> runStateT (f x) s2

instance Monad m => Monad (StateT s m)

instance Monad m => MonadState s (StateT s m) where
  state f = StateT $ pure <<< f

--liftStateT :: forall s m a. Functor m => m a -> StateT s m a
--liftStateT mx = StateT \s -> map (flip Tuple s) mx

instance MonadTrans (StateT s) where
  lift mx = StateT \s -> map (flip Tuple s) mx

instance MonadAsk r m => MonadAsk r (StateT s m) where
  ask = lift ask

instance MonadTell w m => MonadTell w (StateT s m) where
  tell = lift <<< tell

instance MonadThrow e m => MonadThrow e (StateT s m) where
  throwError = lift <<< throwError

instance MonadError e m => MonadError e (StateT s m) where
  catchError :: forall a. StateT s m a -> (e -> StateT s m a) -> StateT s m a
  catchError (StateT fmx) f = StateT \s -> catchError (fmx s) (\e -> runStateT (f e) s)

type AppStack e w s a = ExceptT e (WriterT w (StateT s Effect)) a
type StackResult = Tuple (Tuple (Either String Unit) String) Int

type AppM = AppStack String String Int Unit

type AppEffects =
  { log    :: String
  , state  :: Int
  , result :: Maybe Unit
  }

type AppResult = Tuple (Maybe String) AppEffects

results :: StackResult -> AppResult
results (Tuple (Tuple (Left err) l) s) =
  Tuple (Just err) { log: l, state: s, result: Nothing }
results (Tuple (Tuple (Right result) l) s) =
  Tuple Nothing { log: l, state: s, result: Just result }

runApp :: Int -> AppM -> Effect AppResult
runApp s = map results <<< flip runStateT s <<< runWriterT <<< runExceptT

log :: forall m. MonadTell String m => String -> m Unit
log s = tell $ s <> "\n"

app :: AppM
app = do
  log "starting app.."
  n <- get
  when (n == 0) $ void $ throwError "0 state not allowed!"
  put (n + 1)
  log "incremented state"
  pure unit
--app = do
--  log "starting app..."
--  n <- get
--  -- when (n == 0) $ void $ throwError "0 state is no go!!"
--  catchError (validate n) (\err -> do
--    log $ "error has happened: " <> err
--    put 1
--  )
--  put $ n + 1
--  log "incrment state"
--  pure unit

add :: Int -> Int -> Int
add x y = (spy "X" x) + (spy "Y" y)

main :: Effect Unit
main = do
  res1 <- runApp 0 app
  Console.log $ show res1
  res2 <- runApp 99 app
  Console.log $ show res2
  let foo = add 42 32
  Console.log $ show foo
