module Main where

import Prelude
import Data.Newtype (class Newtype, unwrap)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Effect (Effect)
import Effect.Console (log)

type Address =
  { street :: String
  , city   :: String
  , state  :: String
  , zip    :: String
  }

--derive instance Generic Address _
--instance Show Address where
--  show = genericShow

class HasAddress a where
  getAddress :: a -> Address

newtype Person = Person
  { name    :: String
  , age     :: Int
  , address :: Address
  }
derive instance Generic Person _
instance Show Person where
  show = genericShow

genericPersonHasAddress :: forall a. Newtype a Person => a -> Address
genericPersonHasAddress wrappedPerson =
  getAddress $ unwrap wrappedPerson

instance HasAddress Person where
  getAddress (Person p) = p.address

newtype Ceo = Ceo Person
derive instance Newtype Ceo _
derive newtype instance HasAddress Ceo

newtype Janitor = Janitor Person
derive instance Newtype Janitor _
derive newtype instance HasAddress Janitor

newtype FirstName = FirstName String
derive instance Newtype FirstName _
derive instance Eq FirstName

newtype LastName = LastName String
derive instance Newtype LastName _

glueNames
  :: forall a b
   . Newtype a String
  => Newtype b String
  => String
  -> a
  -> b
  -> String
glueNames between n1 n2 = unwrap n1 <> between <> unwrap n2

fullName :: FirstName -> LastName -> String
fullName = glueNames " "

class IsRecord a where
  isRecord :: a -> Boolean

-- 'instance chaining'
-- to help hint the compiler how to prioritize when resolving the instances
instance IsRecord (Record a) where
  isRecord _ = true
else instance IsRecord a where
  isRecord _ = false

addr :: Address
addr = { street : "sofievej 9, st mf"
       , city   : "Holte"
       , state  : "DK"
       , zip    : "2840"
       }

me :: Person
me = Person { name    : "michel"
            , age     : 35
            , address : addr}

data Maybe a = Nothing | Just a

instance Show a => Show (Maybe a) where
  show Nothing  = "Nothing"
  show (Just x) = "(Just " <> show x <> ")"

main :: Effect Unit
main = do
  log $ show me
  log $ show $ isRecord addr
  log $ show $ isRecord "hello"
  log $ show $ Just 42
  log "🍝"
