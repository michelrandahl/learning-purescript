module Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Data.Tuple (Tuple(..))

newtype Reader r a = Reader (r -> a)
newtype Writer w a = Writer (Tuple a w)
newtype State s a = State (s -> Tuple a s)

type RWSResult r w s =
  { r :: r
  , w :: w
  , s :: s
  }

newtype RWS r w s a = RWS (RWSResult r w s -> Tuple a (RWSResult r w s))

instance Functor (RWS r w s) where
  map :: forall a b. (a -> b) -> RWS r w s a -> RWS r w s b
  map f (RWS g) =
    RWS \rws ->
      g rws
      # \(Tuple x rws') -> Tuple (f x) rws'

runRWS :: forall r w s a. RWS r w s a -> (RWSResult r w s -> Tuple a (RWSResult r w s))
runRWS (RWS f) = f

instance Monoid w => Bind (RWS r w s) where
  bind :: forall a b. RWS r w s a -> (a -> RWS r w s b) -> RWS r w s b
  bind (RWS g) f = RWS \rws ->
    g rws
    # \(Tuple x rws'@{ w }) -> runRWS (f x) rws'
      # \(Tuple y { r, w: w', s }) -> Tuple y { r, w: w <> w', s}

instance Monoid w => Applicative (RWS r w s) where
  pure x = RWS \{r, s} -> Tuple x {r, w: mempty, s}

instance Monoid w => Monad (RWS r w s)

instance Monoid w => Apply (RWS r w s) where
  apply = ap

tell :: forall r w s. w -> RWS r w s Unit
tell w = RWS \{r, s} -> Tuple unit {r, w, s}

ask :: forall r w s. Monoid w => RWS r w s r
ask = RWS \{r, s} -> Tuple r {r, w: mempty, s}

get :: forall r w s. Monoid w => RWS r w s s
get = RWS \{r, s} -> Tuple s {r, w: mempty, s}

put :: forall r w s. Monoid w => s -> RWS r w s Unit
put s = RWS \{r} -> Tuple unit {r, w: mempty, s}


-------------------- testing the RWS monad --------------------
type Config = { debugModeOn :: Boolean }
type Counter = Int

rwsTest :: RWS Config (Array String) Counter Unit
rwsTest = do
  tell ["test the log"]
  tell ["test the log2", "test the log3"]
  config <- ask
  tell ["the config is " <> show config]
  counter <- get
  tell ["old counter is " <> show counter]
  put $ counter + 1
  newCounter <- get
  tell ["new counter is " <> show newCounter]
  pure unit

main :: Effect Unit
main = do
  log $ show $ runRWS rwsTest { r: { debugModeOn: true }, w: mempty, s: 0}

