module Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Bifunctor (class Bifunctor)
import Family (Age(..), createFamilyAges)
import Parser as Parser

data Maybe a = Nothing | Just a

derive instance Generic (Maybe a) _
instance Show a => Show (Maybe a) where
  show = genericShow

instance Functor Maybe where
  map :: forall a b. (a -> b) -> Maybe a -> Maybe b
  map f (Just x) = Just $ f x
  map _ Nothing  = Nothing

instance Apply Maybe where
  apply :: forall a b. Maybe (a -> b) -> Maybe a -> Maybe b
  apply (Just f) x = map f x
  apply Nothing _ = Nothing

instance Applicative Maybe where
  pure :: forall a. a -> Maybe a
  pure = Just

data Either a b = Left a | Right b

derive instance Generic (Either a b) _
instance (Show a, Show b) => Show (Either a b) where
  show = genericShow

derive instance (Eq a, Eq b) => Eq (Either a b)
derive instance (Ord a, Ord b) => Ord (Either a b)
derive instance Functor (Either a)

instance Bifunctor Either where
  bimap :: forall a b c d. (a -> c) -> (b -> d) -> Either a b -> Either c d
  bimap f _ (Left x)  = Left $ f x
  bimap _ g (Right x) = Right $ g x


instance Apply (Either a) where
  apply :: forall b c. Either a (b -> c) -> Either a b -> Either a c
  apply (Right f) x = map f x
  apply (Left x) _ = Left x

-- TODO why do we need Apply implemented to make Applicative?
instance Applicative (Either a) where
  pure :: forall b. b -> Either a b
  pure = Right

main :: Effect Unit
main = do
  log $ show $ (+) <$> Just 21 <*> Just 21
  -- LAW: Associative Composition
  log $ show $ ((<<<) <$> pure identity <*> pure identity <*> pure 1) ==
             -- (<<<) <$> Right identity <*> Right identity <*> Right 1
             -- map (<<<) Right identity <*> Right identity <*> Right 1
             -- Right (identity <<< _) <*> Right identity <*> Right 1
             -- apply (Right (identity <<< _)) Right identity <*> Right 1
             -- Right (identity <<< identity) <*> Right 1
             -- Right identity <*> Right 1
             -- apply (Right identity) (Right 1)
             -- Right 1
               (pure identity <*> (pure identity <*> pure 1) :: Either Unit Int)
  -- LAW: Identity
  log $ show $ (pure identity <*> pure 1) == (pure 1 :: Either Unit Int)
  -- LAW: Homomorphism
  log $ show $ pure (negate 1) == (pure negate <*> pure 1 :: Either Unit Int)
  -- LAW: Interchange
  log $ show $ (pure negate <*> pure 1) == (pure (_ $ 1) <*> pure negate :: Either Unit Int)
  log "🍝"
  log $ show $ createFamilyAges { fatherAge: Age 40, motherAge: Age 30, childAge: Age 10 }
  log $ show $ createFamilyAges { fatherAge: Age 3, motherAge: Age 30, childAge: Age 100 }
  Parser.test
