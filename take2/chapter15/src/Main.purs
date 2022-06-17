module Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Data.Int.Bits ((.&.))
import Data.Functor.Contravariant (class Contravariant, cmap, (>$<))
import Data.Profunctor (class Profunctor, dimap)
import Data.Foldable (class Foldable, foldl)
import Data.List (List(..), (:))
import Data.String (length)

even :: Int -> Boolean
even x = x .&. 1 == 0

odd :: Int -> Boolean
odd x = x .&. 1 == 1

data Predicate a = Predicate (a -> Boolean)

instance Contravariant Predicate where
  cmap :: forall a b. (b -> a) -> Predicate a -> Predicate b
  cmap f (Predicate g) = Predicate $ g <<< f

runPredicate :: forall a. Predicate a -> a -> Boolean
runPredicate (Predicate f) = f

data OvenState = Off | Bake | Idling

data Heat = HeatOn | HeatOff

outputFn :: OvenState -> Heat
outputFn Off    = HeatOff
outputFn Bake   = HeatOn
outputFn Idling = HeatOff

data Moore s a b = Moore s (s -> b) (s -> a -> s)

data InputSignal = BakePressed | OffPressed | TooHot | TooCold

transitionFn :: OvenState -> InputSignal -> OvenState
transitionFn Off BakePressed   = Bake
transitionFn Bake OffPressed   = Off
transitionFn Bake TooHot       = Idling
transitionFn Idling TooCold    = Bake
transitionFn Idling OffPressed = Off
transitionFn s _               = s

instance Profunctor (Moore s) where
  dimap :: forall a b c d. (c -> a) -> (b -> d) -> Moore s a b -> Moore s c d
  dimap f g (Moore s0 output transition) =
    Moore s0 (g <<< output) (\s -> transition s <<< f)

adder :: forall a. Semiring a => Moore a a a
adder = Moore zero identity (+)

runFoldL :: forall s a b (f :: Type -> Type). Foldable f => Moore s a b -> f a -> b
runFoldL (Moore s0 output transition) =
  output <<< foldl transition s0

sizer :: Moore Int String String
sizer = dimap length (\n -> "size is " <> show n) adder

main :: Effect Unit
main = do
  log $ show $ odd 0
  log $ show $ odd 1
  log $ show $ runPredicate (Predicate odd) $ 10
  log $ show $ runPredicate (Predicate odd) $ 11
  log "----------------------------------------"
  log $ show $ runPredicate (cmap (_ + 1) (Predicate odd)) 10
  log $ show $ runPredicate (cmap (_ + 2) (Predicate odd)) 10
  log $ show $ runPredicate ((_ + 1) >$< (Predicate odd)) 10
  log $ show $ runPredicate ((_ + 2) >$< (Predicate odd)) 10
  log "----------------------------------------"
  log $ show $ runFoldL sizer [ "this", "is", "the", "test" ]
  log "🍝"
