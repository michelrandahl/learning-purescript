module Main where

import Prelude (Unit, class Eq, class Show, discard, ($), show, (==), (&&))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (log)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)

class Semigroup a where
  append :: a -> a -> a
infixr 5 append as <>
class Semigroup a <= Monoid a where
  mempty :: a

data AndBool = AFalse | ATrue
derive instance Eq AndBool
derive instance Generic AndBool _
instance Show AndBool where
  show = genericShow

instance Semigroup AndBool where
  append ATrue ATrue = ATrue
  append _ _         = AFalse

instance Monoid AndBool where
  mempty = ATrue

verifyAndBoolSemigroup :: Boolean
verifyAndBoolSemigroup =
  ATrue <> (AFalse <> ATrue) == (ATrue <> AFalse) <> ATrue

verifyAndBoolMonoid :: Boolean
verifyAndBoolMonoid =
  (ATrue <> mempty == mempty <> ATrue) && (mempty == ATrue)

data OrBool = OFalse | OTrue
derive instance Eq OrBool
derive instance Generic OrBool _
instance Show OrBool where
  show = genericShow

instance Semigroup OrBool where
  append OFalse OFalse = OTrue
  append _ _           = OTrue

instance Monoid OrBool where
  mempty = OFalse

data Mod4 = Zero | One | Two | Three
derive instance Eq Mod4
derive instance Generic Mod4 _
instance Show Mod4 where
  show = genericShow

instance Semigroup Mod4 where
  append Zero x      = x
  append x Zero      = x

  append One One     = Two
  append One Two     = Three
  append One Three   = Zero

  append Two One     = Three
  append Two Two     = Zero
  append Two Three   = One

  append Three One   = Zero
  append Three Two   = One
  append Three Three = Two

instance Monoid Mod4 where
  mempty = Zero

class Monoid a <= Group a where
  ginverse :: a -> a

class Semigroup g <= Commutative g

instance Commutative Mod4

instance Group Mod4 where
  ginverse Zero  = Zero
  ginverse One   = Three
  ginverse Two   = Two
  ginverse Three = One

newtype First a = First (Maybe a)

instance Semigroup (First a) where
  append (First Nothing) last = last
  append first  _             = first

instance Monoid (First a) where
  mempty = First Nothing

newtype Last a = Last (Maybe a)

instance Semigroup (Last a) where
  append first (Last Nothing) = first
  append _ last               = last

instance Monoid (Last a) where
  mempty = Last Nothing

hello :: Mod4
hello = Three <> Three

main :: Effect Unit
main = do
  log $ show $ ATrue <> ATrue
  log $ show $ ATrue <> AFalse
  log $ show $ AFalse <> AFalse
  log $ show $ mempty <> AFalse
  log $ show $ mempty <> ATrue
  log "verifyAndBoolSemigroup"
  log $ show verifyAndBoolSemigroup
  log "verifyAndBoolMonoid"
  log $ show verifyAndBoolMonoid
  log $ show hello
  log "🍝"
