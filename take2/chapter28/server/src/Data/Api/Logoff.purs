module Data.Api.Logoff where

import Common.ApiUUID (ApiUUID)
import Control.Monad.Reader.Class (ask)
import Control.Monad.Trans.Class (lift)
import Data.Argonaut.Core (stringify)
import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Decode.Generic (genericDecodeJson)
import Data.Argonaut.Encode.Class (class EncodeJson, encodeJson)
import Data.Argonaut.Encode.Generic (genericEncodeJson)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Debug (spy)
import Entity.Session (Session)
import HTTPure as HTTPure
import Handler.Class.ApiHandler (class RequestHandler, Handler)
import Manager.Session (verifySession, deleteSession)
import Prelude

data LogoffResults = LogoffResultsSuccess | LogoffResultsFailure

derive instance Generic LogoffResults _
instance Show LogoffResults where
  show = genericShow
instance EncodeJson LogoffResults where
  encodeJson = genericEncodeJson
instance DecodeJson LogoffResults where
  decodeJson = genericDecodeJson


newtype LogoffResponse = LogoffResponse LogoffResults

derive instance Generic LogoffResponse _
instance Show LogoffResponse where
  show = genericShow
derive newtype instance EncodeJson LogoffResponse
derive newtype instance DecodeJson LogoffResponse


newtype LogoffRequest = LogoffRequest { authToken :: ApiUUID }

derive instance Generic LogoffRequest _
instance Show LogoffRequest where
  show = genericShow
derive newtype instance EncodeJson LogoffRequest
derive newtype instance DecodeJson LogoffRequest

mySpy :: forall a f. Applicative f => String -> a -> f Unit
mySpy s = void <<< pure <<< spy s

handler :: LogoffRequest -> Handler
handler (LogoffRequest { authToken }) = do
  { sessionsAVar } <- ask
  mySpy "authToken" authToken
  (verifiedSession :: Maybe Session) <- lift $ verifySession sessionsAVar authToken
  mySpy "verifiedSession" verifiedSession
  response <- case verifiedSession of
    Nothing -> pure $ LogoffResponse LogoffResultsFailure
    Just _ -> do
      lift $ deleteSession sessionsAVar authToken
      pure $ LogoffResponse LogoffResultsSuccess
  response
    # encodeJson
    # stringify
    # HTTPure.ok

instance RequestHandler LogoffRequest where
  handle = handler
