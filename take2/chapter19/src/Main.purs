module Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)

data Maybe a = Nothing | Just a
derive instance Generic (Maybe a) _

instance Show a => Show (Maybe a) where
  show = genericShow

instance Functor Maybe where
  map f (Just x) = Just $ f x
  map _ Nothing = Nothing

instance Apply Maybe where
  apply Nothing _ = Nothing
  apply (Just f) x = map f x

instance Applicative Maybe where
  pure = Just
  
instance Bind Maybe where
  bind Nothing _ = Nothing
  bind (Just x) f = f x

instance Monad Maybe


data Either a b = Left a | Right b
derive instance Functor (Either a)
derive instance Generic (Either a b) _

instance (Show a, Show b) => Show (Either a b) where
  show = genericShow

instance Apply (Either a) where
  apply (Left x) _ = Left x
  apply (Right f) x = map f x

instance Applicative (Either a) where
  pure = Right

instance Bind (Either a) where
  bind (Left x) _ = Left x
  bind (Right x) f = f x

instance Monad (Either a)

main :: Effect Unit
main = do
  -------------------- Maybe --------------------
  log $ show $ Just (_ * 10) <*> Just 20
  log $ show $ Just (_ * 10) <*> pure 20
  log $ show $ Just 20 >>= pure <<< (_ * 10)
  log $ show do
    x <- Just 20
    let y = x * 10
    pure y
  log $ show $ Just 20 >>= const Nothing >>= \y -> Just $ y + 42
  log $ show do
    _ <- Just 20
    y <- Nothing
    pure $ y + 42

  -------------------- Either --------------------
  log $ show $ Right (_ * 10) <*> (Right 20 :: Either Unit _)
  log $ show $ Right (_ * 10) <*> (pure 20 :: Either Unit _)
  log $ show $ (Right 20 :: Either Unit _) >>= pure <<< (_ * 10)
  log $ show do
    x <- Right 20 :: Either Unit _
    let y = x * 10
    pure y
  log $ show $ Right 20 >>= const (Left "error") >>= \y -> Right $ y + 42
  log $ show do
    _ <- Right 20
    y <- Left "error"
    pure $ y + 42




