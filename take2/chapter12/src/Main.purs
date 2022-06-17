module Main where

import Prelude

--import Data.EuclideanRing (class EuclideanRing)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (log)
import Data.Bifunctor (class Bifunctor)

divide :: forall a. Eq a => EuclideanRing a => a -> a -> Maybe a
divide x y
  | y == zero = Nothing
  | otherwise = Just $ x / y

myZero :: Int
myZero = zero

myFun :: forall a b c. (b -> c) -> (a -> b) -> a -> c
myFun g f x = g (f x)

data Either a b = Left a | Right b

instance Functor (Either a) where
  map _ (Left x)  = Left x
  map f (Right y) = Right $ f y

data Choice a b = PickA a | PickB b

instance Bifunctor Choice where
  bimap f _ (PickA x) = PickA $ f x
  bimap _ g (PickB y) = PickB $ g y

div2 :: Int -> Int
div2 = (30 / _)

foo :: Maybe Int
foo = div2 <$> Just 10

main :: Effect Unit
main = do
  log $ show $ div2 0
  log $ show foo
  log $ show $ Just 42
  log $ show $ Just 42 == Nothing
  log $ show $ divide 42 3
  log $ show $ divide 42 0
  log $ show myZero
  log "🍝"
