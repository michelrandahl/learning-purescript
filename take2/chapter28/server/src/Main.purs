module Main where

import Common.ApiUUID (genApiUUID)
import Control.Monad.Reader.Trans (runReaderT)
import Data.Argonaut.Decode (parseJson, decodeJson)
import Data.Either (Either(..))
import Data.JSDate (now, getTime, toUTCString)
import Data.Posix.Signal (Signal(SIGINT,SIGTERM))
import Debug (spy)
import Effect (Effect)
import Effect.Aff (launchAff_, Aff)
import Effect.Aff.AVar (AVar)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import HTTPure as HTTPure
import HTTPure.Body (toString)
import HTTPure.Request (Request)
import HTTPure.Response (ResponseM)
import Handler.Account as AccountHandler
import Handler.Api.Common (RequestTypes)
import Handler.Class.ApiHandler (HandlerEnv, handle)
import Manager.Account (Accounts)
import Manager.Account as AccountManager
import Manager.Session (Sessions)
import Manager.Session as SessionManager
import Node.Process (version, onSignal)
import Prelude
import Record (delete)
import Type.Proxy (Proxy(..))

port :: Int
port = 3000

unixEpoch :: Aff Number
unixEpoch = getTime <$> liftEffect now

router :: HandlerEnv -> Request -> ResponseM
router env { body, method }
  | method == HTTPure.Post = do
      bodyString <- toString body
      let (parsed :: Either _ RequestTypes) = parseJson bodyString >>= decodeJson
      case parsed of
        Left err -> HTTPure.badRequest $ "err: " <> (show err)
        Right req -> do
          runReaderT (handle req) env
  | otherwise = HTTPure.methodNotAllowed

loggingRouter :: HandlerEnv -> Request -> ResponseM
loggingRouter env req@{body} = do
  startTime <- liftEffect now --unixEpoch
  uuid <- liftEffect genApiUUID
  let idStr = " (" <> show uuid <> ")"
      ts dt = "(" <> toUTCString dt <> ") "
  log $ ts startTime <> "REQUEST: " <> (show $ delete (Proxy :: _ "body") req) <> idStr
  body' <- toString body
  log $ ts startTime <> "BODY: " <> body' <> idStr

  res <- router env req

  endTime <- liftEffect now --unixEpoch
  log $ ts endTime <> "RESPONSE: " <> (show $ delete (Proxy :: _ "writeBody") res) <> idStr
  let duration = getTime endTime - getTime startTime
  log $ ts endTime <> "TOTAL TIME: " <> show duration <> idStr
  pure res

shutdownServer :: (Effect Unit -> Effect Unit) -> AVar Sessions -> AVar Accounts -> Effect Unit
shutdownServer shutdownFn sessionsAVar accountsAVar = launchAff_ do
  log "shutting down server"
  SessionManager.shutdown sessionsAVar
  AccountManager.shutdown accountsAVar
  liftEffect $ shutdownFn $ log "server shutdown"

main :: Effect Unit
main = launchAff_ do
  log $ "Node version is: " <> version
  accounts <- AccountHandler.loadAccounts
  case accounts of
    Left err -> log $ "Failed to load accounts: " <> show err
    Right accounts' -> do
      accountsAVar <- AccountManager.startup accounts'
      sessionsAVar <- SessionManager.startup
      let handlerEnv = { accountsAVar, sessionsAVar }
      liftEffect $ do
        (shutdown :: Effect Unit -> Effect Unit) <-
          HTTPure.serve port (loggingRouter handlerEnv) (log $ "server started! (" <> show port <> ")")
        log "after server start"
        onSignal SIGINT $ shutdownServer shutdown sessionsAVar accountsAVar
        onSignal SIGTERM $ shutdownServer shutdown sessionsAVar accountsAVar
