module Main where

import Prelude (class Show, Unit, bind, discard, show, ($), unit, pure, (<<<), map, (<>))

import Data.Traversable (sequence)
import Affjax.Node as Ajax
import Affjax.RequestBody as RequestBody
import Affjax.ResponseFormat as ResponseFormat
import Control.Apply ((<*>), (<$>), (<$))
import Control.Alt ((<|>))
import Control.Parallel (parallel, sequential, parSequence)
import Data.Argonaut.Core (stringify, fromString, Json)
import Data.Argonaut.Decode (parseJson)
import Data.Argonaut.Decode.Class (class DecodeJson, decodeJson)
import Data.Argonaut.Decode.Generic (genericDecodeJson)
import Data.Argonaut.Encode.Class (class EncodeJson, encodeJson)
import Data.Argonaut.Encode.Generic (genericEncodeJson)
import Data.Bifunctor (lmap, rmap, bimap)
import Data.Either
import Data.Foldable (oneOf)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.Aff (launchAff_, Aff, delay)
import Effect.Class.Console (log)


newtype Centimeters = Centimeters Number
derive instance Generic Centimeters _
instance Show Centimeters where
  show = genericShow
derive newtype instance EncodeJson Centimeters
derive newtype instance DecodeJson Centimeters
--instance EncodeJson Centimeters where
--  encodeJson (Centimeters v) = encodeJson $ show v
--instance DecodeJson Centimeters where
--  decodeJson json = decodeJson json >>= (pure <<< Centimeters)

newtype Kilograms = Kilograms Number
derive instance Generic Kilograms _
instance Show Kilograms where
  show = genericShow
derive newtype instance EncodeJson Kilograms 
derive newtype instance DecodeJson Kilograms 

newtype Years = Years Int
derive instance Generic Years _
instance Show Years where
  show = genericShow
derive newtype instance EncodeJson Years 
derive newtype instance DecodeJson Years 

newtype GPA = GPA Number
derive instance Generic GPA _
instance Show GPA where
  show = genericShow
derive newtype instance EncodeJson GPA 
derive newtype instance DecodeJson GPA 

type Personal =
  { height :: Centimeters
  , weight :: Kilograms
  , age    :: Years
  }

data Grade = Preschool | Kindergarten | Grade Int | High Int | College Int
derive instance Generic Grade _
instance Show Grade where
  show = genericShow
instance EncodeJson Grade where
  encodeJson = genericEncodeJson
instance DecodeJson Grade where
  decodeJson = genericDecodeJson

data TeachingStatus = Student | Probationary | NonTenured | Tenured
derive instance Generic TeachingStatus _
instance Show TeachingStatus where
  show = genericShow
instance EncodeJson TeachingStatus where
  encodeJson = genericEncodeJson
instance DecodeJson TeachingStatus where
  decodeJson = genericDecodeJson

type Teacher =
  { grades :: Array Grade
  , numberOfStudents :: Int
  , personal :: Personal
  , status :: TeachingStatus
  }

teacher :: Teacher
teacher =
  { grades: [ Preschool, Kindergarten, Grade 1 ]
  , numberOfStudents: 23
  , personal: {
      height: Centimeters 162.56
    , weight: Kilograms 63.5
    , age: Years 31
    }
  , status: NonTenured
  }

student :: Student
student = 
  { grade: Grade 1
  , teacher: teacher
  , gpa: GPA 3.2
  , personal: {
      height: Centimeters 107.9
    , weight: Kilograms 17.9
    , age: Years 5
    }
  }

personalString :: String
personalString = """
  { "height": 181.5
  , "weight": 80.5
  , "age": 36
  }
"""


personalDecoded :: Either _ Personal
personalDecoded = do
  object <- parseJson personalString
  decodeJson object

type Student =
  { grade    :: Grade
  , teacher  :: Teacher
  , gpa      :: GPA
  , personal :: Personal
  }

processAjaxResult :: forall a. Either Ajax.Error { body :: Json | a } -> Aff Unit
processAjaxResult result = do
  case result of
    Left err       -> log $ Ajax.printError err
    Right { body } -> log $ stringify body --log $ show (decodeJson body :: Either _ CreateBlogPostRes)

testUrl :: String
testUrl = "http://localhost:3000/" 

testPost :: Effect Unit
testPost = launchAff_ do
  result <- Ajax.post ResponseFormat.string testUrl (Just $ RequestBody.json $ encodeJson teacher)
  --processAjaxResult result
  log $ show $ bimap Ajax.printError _.body result

doNothing :: Aff Unit
doNothing = sequential $ parallel $ pure unit

--requestWithTimeout :: Number -> Aff (Maybe _)
--requestWithTimeout timeout =
--  sequential $ parallel (Just <$> Ajax.get ResponseFormat.string "https://dr.dk") <|> parallel (Nothing <$ delay (Milliseconds timeout))
-- cleaner alternative to the above
requestWithTimeout :: Number -> Aff (Maybe _)
requestWithTimeout timeout =
  sequential $ oneOf $ parallel <$>
    [ (Just <$> Ajax.get ResponseFormat.string "https://foo.com")
    , (Nothing <$ delay (Milliseconds timeout))
    ]

manyRequests :: Aff (Array _)
manyRequests =
  sequential $
    (\p1 p2 p3 -> [p1, p2, p3])
      <$> parallel (Ajax.get ResponseFormat.string "https://foo.com")
      <*> parallel (Ajax.get ResponseFormat.string "https://bar.com")
      <*> parallel (Ajax.get ResponseFormat.string "https://baz.com")

manyRequests2 :: Aff (Array _)
manyRequests2 =
  parSequence
    [ Ajax.get ResponseFormat.string "https://foo.com"
    , Ajax.get ResponseFormat.string "https://bar.com"
    , Ajax.get ResponseFormat.string "https://baz.com"
    ]

testMultiplePost :: Effect Unit
testMultiplePost = launchAff_ do
  results <- parSequence $ (Ajax.post ResponseFormat.json testUrl <<< Just <<< RequestBody.json)
    <$> [encodeJson teacher, encodeJson student]
  log $ case map (_.body) <$> sequence results of
    Left err -> Ajax.printError err
    Right [teacherJson, studentJson] ->
      -- note decoding fails because I was too lazy to impl the types with the reverse key names which the echo-server returns
      show (decodeJson teacherJson :: _ Teacher) <> "\n\n" <> show  (decodeJson studentJson :: _ Student)
    Right _ -> "unexpected number of results from parallel stuff"

-- start the echo server first in another terminal...
-- node src/echo-server.js
main :: Effect Unit
main = do
  log "🍝"
  log $ stringify $ encodeJson $ Centimeters 181.5
  log $ stringify $ encodeJson $ Grade 42
  let (foo :: Either _ Grade) = decodeJson $ encodeJson $ Grade 42
  log $ show foo
  log $ stringify $ encodeJson teacher
  log $ show personalDecoded
  testPost
  launchAff_ doNothing
  testMultiplePost

