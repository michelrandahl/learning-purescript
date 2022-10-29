module Data.Api.QueryUsers where

import Common.ApiUUID (ApiUUID)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (runExceptT)
import Control.Monad.Reader.Class (ask)
import Control.Monad.Trans.Class (lift)
import Data.Argonaut.Core (stringify)
import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Decode.Generic (genericDecodeJson)
import Data.Argonaut.Encode.Class (class EncodeJson, encodeJson)
import Data.Argonaut.Encode.Generic (genericEncodeJson)
import Data.Either (note, either)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Entity.Account (Account(..))
import Entity.Session (Session(..))
import Entity.User (User(..))
import HTTPure as HTTPure
import Handler.Class.ApiHandler (class RequestHandler, Handler)
import Manager.Account as AccountManager
import Manager.Session (verifySession)
import Prelude
import Record (delete)
import Type.Proxy (Proxy(..))
import Utils (liftSuccess)

newtype QueryUsersRequest = QueryUsersRequest { authToken :: ApiUUID }

derive instance Generic QueryUsersRequest _
instance Show QueryUsersRequest where
  show = genericShow
derive newtype instance EncodeJson QueryUsersRequest
derive newtype instance DecodeJson QueryUsersRequest

handler :: QueryUsersRequest -> Handler
handler (QueryUsersRequest { authToken }) = do
  { accountsAVar, sessionsAVar } <- ask
  result <- lift $ runExceptT do
    Session { userName } <-
      verifySession sessionsAVar authToken
      <#> note NotAuthenticated
      # liftSuccess
    Account { admin } <-
      AccountManager.findAccount accountsAVar userName
      <#> note NotAuthorized
      # liftSuccess
    if not admin then throwError NotAuthorized
    else do
      accounts <- lift $ AccountManager.getAccounts accountsAVar
      let users = accounts <#> \(Account a) ->
            User $ delete (Proxy :: _ "passwordHash") a
      pure $ QueryUsersResultsSuccess { users }
  let ok = HTTPure.ok <<< stringify <<< encodeJson <<< QueryUsersResponse
  result # either
    (ok <<< \reason -> QueryUsersResultsFailure { reason })
    ok

instance RequestHandler QueryUsersRequest where
  handle = handler

data QueryUsersFailureReason = NotAuthorized | NotAuthenticated

derive instance Generic QueryUsersFailureReason _
instance Show QueryUsersFailureReason where
  show = genericShow
instance EncodeJson QueryUsersFailureReason where
  encodeJson = genericEncodeJson
instance DecodeJson QueryUsersFailureReason where
  decodeJson = genericDecodeJson


data QueryUsersResults
  = QueryUsersResultsSuccess { users :: Array User }
  | QueryUsersResultsFailure { reason :: QueryUsersFailureReason }

derive instance Generic QueryUsersResults _
instance Show QueryUsersResults where
  show = genericShow
instance EncodeJson QueryUsersResults where
  encodeJson = genericEncodeJson
instance DecodeJson QueryUsersResults where
  decodeJson = genericDecodeJson


newtype QueryUsersResponse = QueryUsersResponse QueryUsersResults

derive instance Generic QueryUsersResponse _
instance Show QueryUsersResponse where
  show = genericShow
derive newtype instance EncodeJson QueryUsersResponse
derive newtype instance DecodeJson QueryUsersResponse
