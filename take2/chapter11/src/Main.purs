module Main where

import Prelude (Unit, show, type (~>), ($), discard, negate, (+), (<>), (<<<))

import Data.List (List(..), (:), foldl, foldr, foldMap, singleton)
--import Data.Semigroup.Foldable (foldl1)
import Data.Foldable (class Foldable)
import Data.NonEmpty ((:|), NonEmpty)
import Effect (Effect)
import Effect.Console (log)
import Data.Ord (compare, Ordering(..), class Ord)
import Data.Maybe (Maybe(..))
import Data.Semiring (class Semiring, zero)
import Data.Monoid (class Monoid)

reverse :: List ~> List
reverse = foldl (\acc x -> x : acc) Nil

max :: forall a. Ord a => a -> a -> a
max x y =
  case compare x y of
    EQ -> x
    LT -> y
    GT -> x

findMax :: forall a. Ord a => List a -> Maybe a
findMax Nil      = Nothing
findMax (x : xs) = Just $ foldl max x xs

-- NOTE: we have to provide a default value
foldl1 :: forall f a. Foldable f => (a -> a -> a) -> NonEmpty f a -> a
foldl1 f (x :| xs) = foldl f x xs

-- NOTE: we are using the non-empty type, so we can avoid having to provide a default val
findMaxNE :: forall f a. Foldable f => Ord a => NonEmpty f a -> a
findMaxNE = foldl1 max

sum :: forall a f. Semiring a => Foldable f => f a -> a
sum = foldl (+) zero

data Tree a = Leaf a | Node (Tree a) (Tree a)

--toList :: forall a. Tree a -> List a
--toList (Leaf x)   = singleton x
--toList (Node l r) = (toList l) <> (toList r)
--
--instance Foldable Tree where
--  foldr :: forall a b. (a -> b -> b) -> b -> Tree a -> b
--  foldr f b = foldr f b <<< toList
--
--  foldl :: forall a b. (b -> a -> b) -> b -> Tree a -> b
--  foldl f b = foldl f b <<< toList
--
--  foldMap :: forall a m. Monoid m => (a -> m) -> Tree a -> m
--  foldMap f = foldMap f <<< toList

class ToList f where
  toList :: forall a. f a -> List a

newtype RightFirstTree a = RightFirstTree (Tree a)
newtype LeftFirstTree a = LeftFirstTree (Tree a)

instance ToList RightFirstTree where
  toList (RightFirstTree (Leaf x)) = singleton x
  toList (RightFirstTree (Node l r)) =
    toList (RightFirstTree r) <> toList (RightFirstTree l)

instance ToList LeftFirstTree where
  toList (LeftFirstTree (Leaf x)) = singleton x
  toList (LeftFirstTree (Node l r)) =
    toList (LeftFirstTree l) <> toList (LeftFirstTree r)

instance Foldable RightFirstTree where
  foldr f acc = foldr f acc <<< toList
  foldl f acc = foldl f acc <<< toList
  foldMap f = foldMap f <<< toList

instance Foldable LeftFirstTree where
  foldr f acc = foldr f acc <<< toList
  foldl f acc = foldl f acc <<< toList
  foldMap f = foldMap f <<< toList

myTree :: Tree Int
myTree = Node (Node (Leaf 5) (Node (Leaf (-1)) (Leaf 14))) (Leaf 99)

main :: Effect Unit
main = do
  log $ show $ reverse (10 : 20 : 30 : Nil)
  log $ show $ max (-1) 99
  log $ show $ max "aa" "z"
  log $ show $ findMax (37 : 311 : -1 : 2 : 84 : Nil)
  log $ show $ findMax ("a" : "bbb" : "c" : Nil)
  log $ show $ findMaxNE (37 :| (311 : -1 : 2 : 84 : Nil))
  log $ show $ findMaxNE ("a" :| ("bbb" : "c" : Nil))
  log $ show $ sum (1 : 2 : 3 : Nil)
  log $ show $ sum [1.0, 2.0, 3.0]
  log $ show $ toList $ LeftFirstTree myTree
  log $ show $ sum $ RightFirstTree myTree

