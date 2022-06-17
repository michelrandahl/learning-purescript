module Main where

import Prelude
import Data.Foldable (class Foldable, foldl, foldr)

import Effect (Effect)
import Effect.Console (log)

data List a = Cons a (List a) | Nil

instance Foldable List where
  foldr :: forall a b. (a -> b -> b) -> b -> List a -> b
  foldr _ acc Nil         = acc
  foldr f acc (Cons x xs) = f x (foldr f acc xs)

  foldl :: forall a b. (b -> a -> b) -> b -> List a -> b
  foldl _ acc Nil         = acc
  foldl f acc (Cons x xs) = foldl f (f acc x) xs

  foldMap :: forall a m. Monoid m => (a -> m) -> List a -> m
  foldMap _ Nil = mempty
  foldMap f xs  = foldl (\acc x -> acc <> f x) mempty xs

length :: forall a. List a -> Int
length xs = foldl (\acc _ -> acc + 1) 0 xs

main :: Effect Unit
main = do
  log $ show $ length (Cons "1" (Cons "2" (Cons "3" Nil)))
