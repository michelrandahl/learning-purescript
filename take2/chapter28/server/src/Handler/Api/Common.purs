module Handler.Api.Common where

import Data.Api.Logoff as Logoff
import Data.Api.Logon as Logon
import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Decode.Generic (genericDecodeJson)
import Data.Argonaut.Encode.Class (class EncodeJson)
import Data.Argonaut.Encode.Generic (genericEncodeJson)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Handler.Class.ApiHandler (handle, class RequestHandler)
import Prelude
import Data.Api.CreateUser as CreateUser
import Data.Api.QueryUsers as QueryUsers

data RequestTypes
  = LogonRequest Logon.LogonRequest
  | LogoffRequest Logoff.LogoffRequest
  | CreateUserRequest CreateUser.CreateUserRequest
  | QueryUsersRequest QueryUsers.QueryUsersRequest

instance RequestHandler RequestTypes where
  handle (LogonRequest req)      = handle req
  handle (LogoffRequest req)     = handle req
  handle (CreateUserRequest req) = handle req
  handle (QueryUsersRequest req) = handle req

derive instance Generic RequestTypes _
instance Show RequestTypes where
  show = genericShow
instance EncodeJson RequestTypes where
  encodeJson = genericEncodeJson
instance DecodeJson RequestTypes where
  decodeJson = genericDecodeJson
