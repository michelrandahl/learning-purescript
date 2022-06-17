module Main where

import Prelude (Unit, class Show, class Eq, show, discard, ($), (/), (<>), identity, (==), (*), (<<<))

import Effect (Effect)
import Effect.Console (log)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)

import Data.String.Common (toUpper)

class Functor f where
  map :: forall a b. (a -> b) -> f a -> f b

infixl 4 map as <$>

class Bifunctor f where
  bimap :: forall a b c d. (a -> c) -> (b -> d) -> f a b -> f c d

lmap :: forall f a b c. Bifunctor f => (a -> c) -> f a b -> f c b 
lmap f = bimap f identity

rmap :: forall f a b d. Bifunctor f => (b -> d) -> f a b -> f a d
rmap = bimap identity

data Maybe a = Nothing | Just a
derive instance Generic (Maybe a) _
instance Show a => Show (Maybe a) where
  show = genericShow
instance Functor Maybe where
  map _ Nothing  = Nothing
  map f (Just x) = Just (f x)
derive instance Eq a => Eq (Maybe a)

data Either a b = Left a | Right b
derive instance Generic (Either a b) _
instance (Show a, Show b) => Show (Either a b) where
  show = genericShow
instance Functor (Either a) where
  map _ (Left l)  = Left l
  map f (Right r) = Right (f r)
instance Bifunctor Either where
  bimap fl _ (Left l) = Left $ fl l
  bimap _ fr (Right r) = Right $ fr r

data Tuple a b = Tuple a b
derive instance Generic (Tuple a b) _
instance (Show a, Show b) => Show (Tuple a b) where
  show = genericShow
instance Functor (Tuple a) where
  map f (Tuple a b) = Tuple a (f b)
instance Bifunctor Tuple where
  bimap fl fr (Tuple l r) = Tuple (fl l) (fr r)

data Threeple a b c = Threeple a b c
derive instance Generic (Threeple a b c) _
instance (Show a, Show b, Show c) => Show (Threeple a b c) where
  show = genericShow
instance Functor (Threeple a b) where
  map f (Threeple a b c) = Threeple a b (f c)
instance Bifunctor (Threeple a) where
  bimap fl fr (Threeple a b c) = Threeple a (fl b) (fr c)

main :: Effect Unit
main = do
  log $ show $ (_ / 2) <$> Just 10
  log $ show $ (_ / 2) <$> Nothing
  log $ show $ (_ / 2) <$> Left "err"
  log $ show $ (_ / 2) <$> (Right 10 :: Either Unit _)
  log $ show $ (_ / 2) <$> Tuple 10 20
  log $ show $ (_ / 2) <$> Threeple 10 20 40
  log $ show $ rmap (_ * 2) $ Left "error reason"
  log $ show $ rmap (_ * 2) $ (Right 10 :: Either Unit _)
  log $ show $ lmap toUpper $ (Left "err" :: Either _ Unit)
  log $ show $ lmap toUpper $ Right 10
  log "-------------------- Testing Laws of Functor --------------------"
  log $ show $ "Maybe Identity for Nothing: "
    <> show ((identity <$> Nothing) == (Nothing :: Maybe Unit))
  log $ show $ "Maybe Identity for Just: "
    <> show ((identity <$> Just [1, 2]) == Just [1, 2])
  let g x = x * 2
      f x = x * 3
  log $ show $ "Maybe Composition for Nothing: "
    <> show (map (g <<< f) Nothing == (map f <<< map g) Nothing)
  log $ show $ "Maybe Composition for Just: "
    <> show (map (g <<< f) (Just 60) == (map f <<< map g) (Just 60))


