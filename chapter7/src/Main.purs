module Main where

import Prelude (Unit, show, discard, (<<<), (+), (==), ($), (<), (>), (<=), (>=))
import Data.Ord (class Ord, Ordering(..), compare)
import Data.Eq (class Eq)
import Effect (Effect)
import Effect.Console (log)

data Maybe a = Nothing | Just a

instance Eq a => Eq (Maybe a) where
  eq (Just x) (Just y) = x == y
  eq Nothing Nothing = true
  eq _ _ = false

instance Ord a => Ord (Maybe a) where
  compare Nothing Nothing = EQ
  compare (Just x) (Just y) = compare x y
  compare (Just _) Nothing = GT
  compare Nothing (Just _) = LT

foo :: Int -> Int
foo = (+) 42

inc :: Int -> Int
inc = (+) 1

stuff :: Int -> String
stuff = show <<< foo <<< inc

class IsRecord a where
  isRecord :: a -> Boolean

instance IsRecord (Record a) where
  isRecord _ = true
else instance IsRecord a where
  isRecord _ = false

main :: Effect Unit
main = do
  log "🍝"
  log $ show $ Just 5 == Just 5
  log $ show $ Just 5 == Just 4
  log $ show $ Just 5 == Nothing
  log $ show $ Nothing == (Nothing :: Maybe Unit)
