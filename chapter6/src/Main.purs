module Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Data.List (List(..), sort, (:))
import Data.List as List
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Newtype (class Newtype, unwrap)
import Data.String.CodeUnits as String
import Data.String.CodePoints (CodePoint)
import Data.String as StringUnicode
import Data.Maybe (Maybe(..))
import Data.Array as Array

data Place = First
           | Second
           | Third

instance eqPlace :: Eq Place where
  eq First First = true
  eq Second Second = true
  eq Third Third = true
  eq _ _ = false

instance Ord Place where
  compare First First = EQ
  compare First _ = LT
  compare Second Second = EQ
  compare Second Third = LT
  compare Second First = GT
  compare Third Third = EQ
  compare Third _ = GT

instance showPlace :: Show Place where
  show First = "First"
  show Second = "Second"
  show Third = "Third"

type MyAddress =
  { street :: String
  , zipcode :: String }

type MyPerson =
  { name :: String
  , phone :: String
  , address :: MyAddress }

type MyCompany =
  { ceo :: MyPerson
  , name :: String
  , phone :: String
  , address :: MyAddress }

-- rowtype polymorphism
getZipcode :: forall x. { address :: MyAddress | x } -> String
getZipcode = _.address.zipcode

-- rowtype polymorphism
updatePhone :: forall x. String -> { phone :: String | x} -> { phone :: String | x}
updatePhone newPhone = _ { phone = newPhone }

someone :: MyPerson
someone = {name: "mnie", phone: "1234", address: {street: "localcoast", zipcode: "1337"}}

somecompany :: MyCompany
somecompany = { ceo: someone, name: "awesome A/S", phone: "5678", address: {street: "newplace", zipcode: "4242"}}

company_zip :: String
company_zip = getZipcode somecompany

personzip :: String
personzip = getZipcode someone

data SomeType = This | That | TheOther | SomethingElse

derive instance Eq SomeType
derive instance Ord SomeType
derive instance Generic SomeType _
instance Show SomeType where
  show = genericShow

newtype Address = Address
  { street :: String
  , zipcode :: String}

derive newtype instance Show Address

newtype Person = Person
  { name :: String
  , age :: Int
  , address :: Address}

derive newtype instance Show Person

class HasAddress a where
  getAddress :: a -> Address

genericPersonHasAddress :: forall a. Newtype a Person => a -> Address
genericPersonHasAddress wrappedPerson =
  getAddress $ unwrap wrappedPerson

instance hasAddressPerson :: HasAddress Person where
  getAddress (Person p) = p.address

newtype Ceo = Ceo Person
derive instance newtypeCeo :: Newtype Ceo _
derive newtype instance hasAddressCeo :: HasAddress Ceo

newtype Janitor = Janitor Person
derive instance newtypeJanitor :: Newtype Janitor _
derive newtype instance hasAddressJanitor :: HasAddress Janitor

class IsRecord a where
  isRecord :: a -> Boolean

instance IsRecord (Record a) where
  isRecord _ = true
else instance IsRecord a where
  isRecord _ = false

class Decapitate collection element | collection -> element where
  decapitate :: collection -> Maybe {head :: element, tail :: collection}

genericTail :: forall collection element. Decapitate collection element => collection -> Maybe collection
genericTail xs =
  case decapitate xs of
      Just { tail } -> Just tail
      Nothing -> Nothing

instance Decapitate String Char where
  decapitate = String.uncons
else instance Decapitate String CodePoint where
  decapitate = StringUnicode.uncons

instance Decapitate (List a) a where
  decapitate = List.uncons
instance Decapitate (Array a) a where
  decapitate = Array.uncons

ta :: Maybe (Array String)
ta = genericTail ["abc", "foo"]

tl :: Maybe (List String)
tl = genericTail ("abc" : "Bar" : Nil)

concat :: String -> String -> String
concat = append

example ::  forall a b. a -> b -> (forall c. c -> String) -> String
example a b function = concat (function a) (function b)

testExample :: String
testExample = example true 5 (const "Foo")

main :: Effect Unit
main = do
  log "🍝"
  log $ show $ sort (First : Third : Second : First : Nil)
  log $ show This
  log $ show $ Address {street: "sofievej", zipcode: "2840"}
  log $ show $ isRecord $ {street: "sofievej", zipcode: "2840"}
  log $ show $ isRecord 42
  log $ show $ genericTail "foobar"
  log $ show $ genericTail $ String.fromCharArray $ String.toCharArray "hello"
  log $ show $ genericTail $ StringUnicode.fromCodePointArray $ StringUnicode.toCodePointArray "hello"
  log $ show ta
  log $ show tl
