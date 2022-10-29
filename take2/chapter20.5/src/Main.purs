module Main where

import Prelude

import Effect (Effect)
import Effect.Console as Console
import Control.Monad.Except.Trans (ExceptT, throwError, runExceptT)
import Control.Monad.State.Trans (StateT, get, put, runStateT)
import Control.Monad.Writer.Trans (WriterT, tell, class MonadTell, runWriterT)
import Data.Identity (Identity)
import Data.Tuple (Tuple(..))
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)

--type AppStack e w s a = ExceptT e (WriterT w (StateT s Identity)) a
-- ETA-reduces to
type AppStack e w s = ExceptT e (WriterT w (StateT s Effect))
type AppM = AppStack String String Int Unit
type StackResult = Tuple (Tuple (Either String Unit) String) Int
type AppEffects = { log :: String , state :: Int , result :: Maybe Unit }
type AppResult = Tuple (Maybe String) AppEffects


log :: forall m. MonadTell String m => String -> m Unit
log s = tell $ s <> "\n"

app :: AppM
app = do
  log "starting app...."
  n <- get
  when (n == 0) $ void $ throwError "dont like 0 state!!"
  put $ n + 1
  log "state incremented"
  pure unit


results :: StackResult -> AppResult
results (Tuple (Tuple (Left err) l) s) = Tuple (Just err) { log : l, state : s, result : Nothing }
results (Tuple (Tuple (Right _) l) s) = Tuple Nothing { log : l, state : s, result : Just unit }

runApp :: Int -> AppM -> Effect AppResult
runApp s = map results <<< flip runStateT s <<< runWriterT <<< runExceptT

main :: Effect Unit
main = do
  x <- runApp 0 app
  Console.log $ show x
  y <- runApp 99 app
  Console.log $ show y
