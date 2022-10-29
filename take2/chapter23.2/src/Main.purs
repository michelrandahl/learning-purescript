module Main where

import Prelude

import Effect (Effect)
--import Effect.Console (log)
import Effect.Class.Console (log)
import Effect.Random (random)
import Effect.Aff (Aff, launchAff_, forkAff, delay)
import Effect.Class (liftEffect)
import Effect.Aff.Bus (BusRW)
import Effect.Aff.Bus as Bus
import Control.Monad.Reader.Trans (ReaderT, runReaderT, ask)
import Control.Monad.State.Trans (StateT, runStateT, get, put, modify_)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Rec.Class (forever)
import Data.Time.Duration (Milliseconds(..))
--import Data.Tuple (Tuple)

type Config = { bus :: BusRW String }
type State = { count :: Int }

-- monad stack: Reader | State | Aff
type FiberM a = ReaderT Config (StateT State Aff) a

--runFiberM :: BusRW String -> FiberM Unit -> Aff (Tuple Unit State)
runFiberM :: BusRW String -> FiberM Unit -> Aff Unit
runFiberM bus =
  void <<< forkAff
  <<< flip runStateT { count : 10 }
  <<< flip runReaderT { bus }

randomAff :: Aff Number
randomAff = liftEffect random

delayRandom :: Aff Number
delayRandom = delay (Milliseconds 1000.0) *> randomAff

-- the complicated way of lifting the random `Effect Number` to `Aff Number`
-- randomAff2 :: Aff Number
-- randomAff2 = makeAff \f -> do
--   n <- random
--   f $ Right n
--   pure nonCanceler

liftAffToFiberM :: Aff ~> FiberM
liftAffToFiberM = lift <<< lift

logger :: FiberM Unit
logger = forever do
  { bus } <- ask
  s <- liftAffToFiberM $ Bus.read bus
  log $ "Logger: " <> s

randomGenerator :: String -> (Number -> Boolean) -> FiberM Unit
randomGenerator valueType pred = do
  { count } <- get
  unless (count <= 0) do
    { bus } <- ask
    liftAffToFiberM do
      n <- delayRandom
      when (pred n) $ flip Bus.write bus $ "found a value that is " <> valueType <> "(" <> show n <> ")"
    modify_ (const { count : count - 1 })
    randomGenerator valueType pred

test :: Effect Unit
test = launchAff_ do
  bus <- Bus.make
  let forkFiberM = runFiberM bus
  forkFiberM logger
  forkFiberM $ randomGenerator "greater than 0.5" (_ > 0.5)
  forkFiberM $ randomGenerator "less than 0.5"    (_ < 0.5)
  forkFiberM $ randomGenerator "greater than 0.1" (_ > 0.1)
  

main :: Effect Unit
main = do
  launchAff_ do
    x <- randomAff
    log $ show x
  test

