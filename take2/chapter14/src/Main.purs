module Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.String.CodeUnits (length)
import Data.Maybe (Maybe(..))
import Data.Either (Either(..), note, hush)

--import FunctorFun

newtype Name = Name String

class Contravariant f where
  cmap :: forall a b. (b -> a) -> f a -> f b

data C1 a = C1 (a -> Int)

instance Contravariant C1 where
  cmap f (C1 g) = C1 (g <<< f)

bla :: C1 String
bla = C1 length

something :: forall a. Show a => C1 a
something = cmap show bla

some :: forall a. Show a => a -> Int
some =
  case something of
      C1 f -> f

joe :: Name
joe = Name "Joe"

joeLen :: Int
joeLen = some joe

data F1 a = F1 (Int -> a)

derive instance Generic Name _
instance Show Name where
  show = genericShow

instance Functor F1 where
  map f (F1 g) = F1 (f <<< g)

foo :: F1 String
foo = F1 (\x -> show x)

bar :: F1 Name
bar = map Name foo

stuff :: Int -> Name
stuff =
  case bar of
      F1 f -> f

foobar :: Name
foobar = stuff 42

newtype F4 a = F4 ((a -> Int) -> Int)

instance Functor F4 where
  map :: forall a b. (a -> b) -> F4 a -> F4 b
  map f (F4 g) = F4 \h -> g (h <<< f)

newtype F5 a = F5 ((Int -> a) -> Int)

instance Contravariant F5 where
  cmap f (F5 g) = F5 \h -> g (f <<< h)

-- natural transformation
hom :: Maybe ~> Either Unit
hom Nothing  = Left unit
hom (Just x) = Right x

maybeToEither :: Maybe ~> Either Unit
maybeToEither Nothing  = Left unit
maybeToEither (Just x) = Right x

eitherToMaybe :: Either Unit ~> Maybe
eitherToMaybe (Left _) = Nothing
eitherToMaybe (Right x)   = Just x

data Iso a b = Iso (a -> b) (b -> a)

class Invariant f where
  imap :: forall a b. (a -> b) -> (b -> a) -> f a -> f b

iso :: forall f a b. Invariant f => Iso a b -> f a -> f b
iso (Iso to from) = imap to from

newtype Endo a = Endo (a -> a)

instance Invariant Endo where
  imap ab ba (Endo aa) = Endo (ab <<< aa <<< ba)

positive :: Maybe Int -> Maybe Int
positive x = if x >= Just 0 then x else Nothing

isoMaybeEither :: String -> Iso (Maybe Int) (Either String Int)
isoMaybeEither err = Iso (note err) hush

positiveEither :: Either String Int -> Either String Int
positiveEither = f
  where
    (Endo f) = iso (isoMaybeEither "Not a positive integer") (Endo positive)

data Tuple a b = Tuple a b

-- creating an infix type operator
infixr 6 type Tuple as &
tup :: forall a b. a -> b -> a & b
tup x y = Tuple x y

main :: Effect Unit
main = do
  log $ show foobar
  log $ show $ positiveEither $ Right 10
  log $ show $ positiveEither $ Right (0 - 10)
  log $ show $ positiveEither $ Left "something?"
  log "🍝"
