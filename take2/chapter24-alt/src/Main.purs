module Main where

import Prelude

import Affjax.RequestBody as RequestBody
import Affjax.Node as Ajax
import Affjax.ResponseFormat as ResponseFormat
import Data.Argonaut.Core (Json, stringify, fromString, jsonEmptyObject)
import Data.Argonaut.Decode (parseJson)
import Data.Argonaut.Decode.Class (class DecodeJson, decodeJson)
import Data.Argonaut.Decode.Combinators ((.:), (.:?), (.!=))
import Data.Argonaut.Decode.Error (JsonDecodeError(..))
import Data.Argonaut.Encode.Class (class EncodeJson, encodeJson)
import Data.Argonaut.Encode.Combinators ((:=), (:=?), (~>?), (~>))
import Data.Either (Either(..), note)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Effect (Effect)
import Effect.Aff (launchAff_, Aff)
import Effect.Class.Console (log)
--import Effect.Console (log)

newtype AppUser = AppUser
  { name :: String
  , age :: Maybe Int
  , team :: Team
  }
derive instance Generic AppUser _
instance Show AppUser where
  show = genericShow

data Team = RedTeam | BlueTeam

derive instance Generic Team _
instance Show Team where
  show = teamToString

teamToString :: Team -> String
teamToString = case _ of
  RedTeam -> "Red Team"
  BlueTeam -> "Blue Team"

teamFromString :: String -> Maybe Team
teamFromString = case _ of
  "Red Team" -> Just RedTeam
  "Blue Team" -> Just BlueTeam
  _ -> Nothing

instance EncodeJson Team where
  encodeJson team = encodeJson (teamToString team)
instance DecodeJson Team where
  decodeJson json = do
    string <- decodeJson json
    note (TypeMismatch "Team") (teamFromString string)

encodedTeam :: Json
encodedTeam = encodeJson BlueTeam

decodedTeam :: Either JsonDecodeError Team
decodedTeam = decodeJson $ fromString "Blue Team"

instance decodeJsonAppUser :: DecodeJson AppUser where
  decodeJson json = do
    obj <- decodeJson json              -- decode `Json` to `Object Json`
    name <- obj .: "name"               -- decode the "name" key to a `String`
    age <- obj .:? "age"                -- decode the "age" key to a `Maybe Int`
    team <- obj .:? "team" .!= RedTeam  -- decode "team" to `Team`, defaulting to `RedTeam`
                                        -- if the field is missing or `null`
    pure $ AppUser { name, age, team }

instance encodeJsonAppUser :: EncodeJson AppUser where
  encodeJson (AppUser { name, age, team }) =
    "name" := name       -- inserts "name": "Tom"
      ~> "age" :=? age   -- inserts "age": "25" (if Nothing, does not insert anything)
      ~>? "team" := team -- inserts "team": "Red Team"
      ~> jsonEmptyObject

decodedUser :: Either JsonDecodeError AppUser
decodedUser = do
  x <- parseJson """{"name": "joe", "age": 42}"""
  decodeJson x

encodedUser :: Json
encodedUser = encodeJson $ AppUser { name : "foo", age : Just 42, team : RedTeam }

-- AJAX

type GetPostRes =
  { id     :: Int
  , title  :: String
  , body   :: String
  , userId :: Int
  }

decodedPostRes :: Either JsonDecodeError GetPostRes
decodedPostRes = do
  object <- parseJson """{"id": 42, "title": "foobar", "body": "and spirit", "userId": 1337}"""
  decodeJson object

url :: String
url = "https://jsonplaceholder.typicode.com/posts/1"

testAjaxGet :: Effect Unit
testAjaxGet =  launchAff_ do
  result <- Ajax.get ResponseFormat.json url
  processAjaxResult result
  --case result of
  --  Left err       -> log $ Ajax.printError err
  --  Right { body } -> log $ show (decodeJson body :: Either _ GetPostRes)

urlPost :: String
urlPost = "https://jsonplaceholder.typicode.com/posts"

type CreateBlogPostReq =
  { title  :: String
  , body   :: String
  , userId :: Int
  }

type CreateBlogPostRes =
  { id     :: Int
  , title  :: String
  , body   :: String
  , userId :: Int
  }

encodedPostReq :: Json
encodedPostReq = encodeJson { userId : 1, title : "title", completed : false}

processAjaxResult :: forall a. Either Ajax.Error { body :: Json | a } -> Aff Unit
processAjaxResult result = do
  case result of
    Left err       -> log $ Ajax.printError err
    Right { body } -> log $ stringify body --log $ show (decodeJson body :: Either _ CreateBlogPostRes)

testAjaxPost :: Effect Unit
testAjaxPost = launchAff_ do
  result <- Ajax.post ResponseFormat.json urlPost (Just $ RequestBody.json encodedPostReq)
  processAjaxResult result
  --case result of
  --  Left err       -> log $ Ajax.printError err
  --  Right { body } -> log $ stringify body --log $ show (decodeJson body :: Either _ CreateBlogPostRes)

main :: Effect Unit
main = do
  log "🍝"
  log $ stringify encodedTeam
  log $ show $ decodedTeam
  log $ show $ decodedUser
  log $ stringify encodedUser
  log $ show decodedPostRes
  testAjaxGet
  testAjaxPost

