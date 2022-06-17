module FunctorFun where

import Prelude ((<<<), (>=))
import Data.Either (Either, note, hush)
import Data.Maybe (Maybe(..))

class Functor f where
  map :: forall a b. (a -> b) -> f a -> f b

instance Functor ((->) r) where
  --f :: a2 -> b1
  --g :: r0 -> a2
  --result: r0 -> b1
  map f g = f <<< g

class Bifunctor f where
  bimap :: forall a b c d. (a -> b) -> (c -> d) -> f a c -> f b d

-- NOTE we cannot write a bifunctor for (->)
-- because (->) is Contravariant in a and Covariant in b.
--instance Bifunctor (->) where
--  --f :: a2 -> b0
--  --g :: c3 -> d1
--  --h :: a2 -> c3
--  --result: b0 -> d1
--  bimap f g h = ?foo

class Profunctor p where
  dimap :: forall a b c d. (b -> a) -> (c -> d) -> p a c -> p b d

instance Profunctor (->) where
  dimap :: forall a b c d. (b -> a) -> (c -> d) -> (a -> c) -> (b -> d)
  dimap f g h = g <<< h <<< f

--lcmap :: forall a b c p. Profunctor p => (b -> a) -> p a c -> p b c
--rmap :: forall a c d p. Profunctor p => (c -> d) -> p a c -> p a d

data Iso a b = Iso (a -> b) (b -> a)

isoP :: forall a b p. Profunctor p => Iso a b -> p a a -> p b b
isoP (Iso to from) = dimap from to

isoPFn :: forall a b. Iso a b -> (a -> a) -> (b -> b)
isoPFn (Iso to from) = dimap from to

positive :: Maybe Int -> Maybe Int
positive x = if x >= Just 0 then x else Nothing

isoMaybeEither :: String -> Iso (Maybe Int) (Either String Int)
isoMaybeEither err = Iso (note err) hush

positiveEither :: Either String Int -> Either String Int
positiveEither = isoPFn (isoMaybeEither "not pos int") positive
-- isoPFn (Iso (Maybe b → Either a b) (Either a b → Maybe b))
--        (Maybe Int -> Maybe Int)
-- dimap (Either a b → Maybe b) (Maybe b → Either a b) (Maybe Int -> Maybe Int)
-- (Maybe b → Either a b) <<< (Maybe Int -> Maybe Int) <<< (Either a b → Maybe b)
-- Either String Int -> Either String Int
