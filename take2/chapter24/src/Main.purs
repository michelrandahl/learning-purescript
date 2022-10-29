module Main where

import Prelude

import Data.Argonaut.Decode.Combinators ((.:))
import Data.Argonaut.Core (stringify, fromString)
import Data.Argonaut.Decode.Parser (parseJson)
import Data.Argonaut.Decode (JsonDecodeError(..))
import Data.Argonaut.Decode.Class (class DecodeJson, decodeJson)
import Data.Argonaut.Encode.Class (class EncodeJson, encodeJson)
import Data.Argonaut.Encode.Generic (genericEncodeJson)
import Data.Argonaut.Decode.Generic (genericDecodeJson)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Effect (Effect)
import Effect.Console (log)
import Data.Either (Either(..))

data Dessert = IceCream | Cake | Pie
data MainDish = Steak | Chicken
data SideDish = Rice | Asparagus
type Meal =
  { main    :: MainDish
  , side    :: SideDish
  , dessert :: Dessert
  }
type Meals = { meals :: Array Meal }

newtype MyDessert = MyDessert { food :: Dessert }

--instance EncodeJson MyDessert where
--  encodeJson (MyDessert r) = encodeJson r
instance DecodeJson MyDessert where
  decodeJson json = do
    x <- decodeJson json
    food <- x .: "food"
    pure $ MyDessert { food }

derive instance Generic MyDessert _
instance Show MyDessert where
  show = genericShow


derive instance Generic Dessert _
instance Show Dessert where
  show = genericShow
instance EncodeJson Dessert where
  encodeJson = encodeJson <<< show 
instance DecodeJson Dessert where
  decodeJson json = do
    x <- decodeJson json
    case x of
      "Ice Cream" -> Right IceCream
      _ -> Left $ TypeMismatch "Dessert"
    -- let x = stringify json in
    --   case x of
    --     "Ice Cream" -> Right IceCream
    --     _ -> Left $ TypeMismatch "no ice cream for u"

--instance DecodeJson Dessert where
--  decodeJson = genericDecodeJson

derive instance Generic MainDish _
instance Show MainDish where
  show = genericShow
instance EncodeJson MainDish where
  encodeJson = genericEncodeJson
instance DecodeJson MainDish where
  decodeJson = genericDecodeJson

derive instance Generic SideDish _
instance Show SideDish where
  show = genericShow
instance EncodeJson SideDish where
  encodeJson = genericEncodeJson
instance DecodeJson SideDish where
  decodeJson = genericDecodeJson

myMeals :: Meals
myMeals = { meals : [{ main : Steak, side : Rice, dessert : Cake }] }

myDessert :: Either JsonDecodeError MyDessert
myDessert = do
  x <- parseJson """{"food": "Ice Cream"}"""
  decodeJson x
--myDessert = do
--  x <- parseJson """{"food": "Ice Cream"}"""
--  decodeJson x

main :: Effect Unit
main = do
  log "🍝"
  --log $ stringify $ encodeJson myMeals
  log $ show myDessert
