module Ch7a where

import Prelude (Unit, show, discard, (==), ($), (<), (>), (<=), (>=))
import Data.Eq (class Eq)
import Data.Ord (class Ord)
import Effect (Effect)
import Effect.Console (log)
import Data.Show (class Show)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)

data Maybe a = Nothing | Just a
derive instance Eq a => Eq (Maybe a)
derive instance Ord a => Ord (Maybe a)
derive instance Generic (Maybe a) _
instance Show a => Show (Maybe a) where
  show = genericShow

--instance Eq a => Eq (Maybe a) where
--  eq (Just l) (Just r) = l == r
--  eq Nothing Nothing   = true
--  eq _ _               = false
--
--instance Ord a => Ord (Maybe a) where
--  compare Nothing Nothing   = EQ
--  compare (Just l) (Just r) = compare l r
--  compare Nothing _         = LT
--  compare _ Nothing         = GT
--
--instance Show a => Show (Maybe a) where
--  show (Just x) = "(Just " <> show x <> ")"
--  show Nothing  = "Nada"

--greaterThanOrEq :: forall a. Ord a => a -> a -> Boolean
--greaterThanOrEq l r =
--  case compare l r of
--    EQ -> true
--    LT -> false
--    GT -> true
--
--infixl 4 greaterThanOrEq as >=

data Either a b = Left a | Right b
derive instance (Eq a, Eq b) => Eq (Either a b)
derive instance (Ord a, Ord b) => Ord (Either a b)
derive instance Generic (Either a b) _
instance (Show a, Show b) => Show (Either a b) where
  show = genericShow

test :: Effect Unit
test = do
  log $ show $ Just "abc"
  log $ show $ (Nothing :: Maybe Unit)
  log "---------- Eq ----------"
  log $ show $ Just 5 == Just 5
  log $ show $ Just 5 == Just 2
  log $ show $ Just 5 == Nothing
  log $ show $ Nothing == Just 5
  log $ show $ Nothing == (Nothing :: Maybe Unit)
  log "---------- Ord ----------"
  log $ show $ Just 1 < Just 5
  log $ show $ Just 5 <= Just 5
  log $ show $ Just 5 > Just 10
  log $ show $ Just 10 >= Just 10
  log $ show $ Just 99 > Nothing
  log $ show $ Just 99 < Nothing
  log "---------- Either ----------"
  log $ show $ (Left "left" :: Either _ Unit)
  log $ show $ (Right (Just 42) :: Either Unit _)

