module Data.Api.Logon where

import Common.ApiUUID (ApiUUID)
import Control.Monad.Reader (ask)
import Control.Monad.Trans.Class (lift)
import Data.Argonaut.Core (stringify)
import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Decode.Generic (genericDecodeJson)
import Data.Argonaut.Encode.Class (class EncodeJson, encodeJson)
import Data.Argonaut.Encode.Generic (genericEncodeJson)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Effect.Aff (Aff)
import Effect.Aff.AVar (AVar)
import Entity.Account (Account(..))
import HTTPure as HTTPure
import Handler.Class.ApiHandler (class RequestHandler, Handler)
import Manager.Account (verifyLogon)
import Manager.Session (Sessions, createSession)
import Prelude


data LogonResults
  = LogonResultSuccess { authToken :: ApiUUID, mustChangePassword :: Boolean }
  | LogonResultsFailure

derive instance Generic LogonResults _
instance Show LogonResults where
  show = genericShow
instance EncodeJson LogonResults where
  encodeJson = genericEncodeJson
instance DecodeJson LogonResults where
  decodeJson = genericDecodeJson


newtype LogonResponse = LogonResponse LogonResults

derive instance Generic LogonResponse _
instance Show LogonResponse where
  show = genericShow
derive newtype instance EncodeJson LogonResponse
derive newtype instance DecodeJson LogonResponse


newtype LogonRequest = LogonRequest
  { userName :: String
  , password :: String
  }

derive instance Generic LogonRequest _
derive instance Newtype LogonRequest _
instance Show LogonRequest where
  show = genericShow
derive newtype instance EncodeJson LogonRequest
derive newtype instance DecodeJson LogonRequest

-- TODO change LogonRequest to normal data type, create the newtype and handler instance in Handler/Api/Logon.purs
handler :: LogonRequest -> Handler
handler (LogonRequest { userName, password }) = do
  { accountsAVar, sessionsAVar } <- ask
  verifyResult :: Maybe Account <- lift $ verifyLogon accountsAVar userName password
  logonResponse <- lift $ mkLogonResponse sessionsAVar verifyResult
  logonResponse
    # encodeJson
    # stringify
    # HTTPure.ok
  where
    mkLogonResponse :: AVar Sessions -> Maybe Account -> Aff LogonResponse
    mkLogonResponse sessionsAVar (Just (Account {temporaryPassword})) = do
      authToken <- createSession sessionsAVar userName
      pure $ LogonResponse $ LogonResultSuccess $
        { mustChangePassword : temporaryPassword
        , authToken          : authToken
        }
    mkLogonResponse _ Nothing = pure $ LogonResponse LogonResultsFailure

instance RequestHandler LogonRequest where
  handle = handler
