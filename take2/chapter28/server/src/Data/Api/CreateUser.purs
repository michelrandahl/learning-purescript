module Data.Api.CreateUser where

import Utils (liftSuccess)
import Common.ApiUUID (ApiUUID)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (runExceptT)
import Control.Monad.Except.Trans (withExceptT)
import Control.Monad.Reader.Class (ask)
import Control.Monad.Trans.Class (lift)
import Data.Argonaut.Core (stringify)
import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Decode.Generic (genericDecodeJson)
import Data.Argonaut.Encode.Class (encodeJson, class EncodeJson)
import Data.Argonaut.Encode.Generic (genericEncodeJson)
import Data.Either (either, note)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Effect.Class (liftEffect)
import Entity.Account (Account(..))
import Entity.Session (Session(..))
import Entity.User (UserRow)
import HTTPure as HTTPure
import Handler.Account as AccountHandler
import Handler.Class.ApiHandler (class RequestHandler, Handler)
import Manager.Account (createAccount, findAccount)
import Manager.Session (verifySession)
import MyCrypto.Crypto (passwordHashHex)
import Prelude
import Record (delete, insert)
import Type.Proxy (Proxy(..))


newtype CreateUserRequest = CreateUserRequest
  { authToken :: ApiUUID
  , user      :: Record (UserRow ( password :: String ))
  }

derive instance Generic CreateUserRequest _
instance Show CreateUserRequest where
  show = genericShow
derive newtype instance EncodeJson CreateUserRequest
derive newtype instance DecodeJson CreateUserRequest

handler :: CreateUserRequest -> Handler
handler (CreateUserRequest { authToken, user }) = do
  { accountsAVar, sessionsAVar } <- ask
  result <- lift $ runExceptT do
    Session { userName } <- verifySession sessionsAVar authToken
      <#> note NotAuthenticated
      # liftSuccess
    Account { admin } <- findAccount accountsAVar userName
      <#> note NotAuthorized
      # liftSuccess
    unless admin $ throwError NotAuthorized
    passwordHash <- liftEffect $ passwordHashHex user.userName user.password
    let newUser = delete (Proxy :: _ "password") user
        account = Account $ insert (Proxy :: _ "passwordHash") passwordHash newUser
    createAccount accountsAVar account
      # liftSuccess
      # (withExceptT $ const AlreadyExists)
    AccountHandler.createAccount account
      # liftSuccess
      # withExceptT \(AccountHandler.CreateAccountFileError err) -> FileIOError err
  HTTPure.ok $ stringify $ encodeJson $ CreateUserResponse $
    result # either
      (\reason -> CreateUserResultsFailure { reason })
      (const CreateUserResultsSuccess)

instance RequestHandler CreateUserRequest where
  handle = handler


data CreateUserFailureReason
  = NotAuthorized
  | NotAuthenticated
  | AlreadyExists
  | FileIOError String

derive instance Generic CreateUserFailureReason _
instance Show CreateUserFailureReason where
  show = genericShow
instance EncodeJson CreateUserFailureReason where
  encodeJson = genericEncodeJson
instance DecodeJson CreateUserFailureReason where
  decodeJson = genericDecodeJson


data CreateUserResults
  = CreateUserResultsSuccess
  | CreateUserResultsFailure { reason :: CreateUserFailureReason }

derive instance Generic CreateUserResults _
instance Show CreateUserResults where
  show = genericShow
instance EncodeJson CreateUserResults where
  encodeJson = genericEncodeJson
instance DecodeJson CreateUserResults where
  decodeJson = genericDecodeJson

newtype CreateUserResponse = CreateUserResponse CreateUserResults
derive newtype instance EncodeJson CreateUserResponse
derive newtype instance DecodeJson CreateUserResponse

