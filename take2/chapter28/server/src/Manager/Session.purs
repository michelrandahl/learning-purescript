module Manager.Session where


import Utils (withAVar)
import Common.ApiUUID (ApiUUID, genApiUUID)
import Data.JSDate (now, getTime)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Effect.Aff.AVar (AVar)
import Effect.Aff.AVar as AVar
import Effect.Class (liftEffect)
import Entity.Session (Session(..))
import Prelude

type Sessions = Map ApiUUID Session

shutdown :: AVar Sessions -> Aff Unit
shutdown = void <<< AVar.take

startup :: Aff (AVar Sessions)
startup = AVar.new Map.empty

sessionTimeout :: Number
sessionTimeout = 4.0 * 60.0 * 60.0 * 1000.0 -- 4 hours

unixEpoch :: Aff Number
unixEpoch = getTime <$> liftEffect now

sessionIsExpired :: Number -> Session -> Boolean
sessionIsExpired currentTime (Session { lastTime }) =
  let duration = currentTime - lastTime
  in duration >= sessionTimeout

expireSessions :: AVar Sessions -> Aff Unit
expireSessions sessionsAVar = do
  currentTime <- unixEpoch
  sessions <- AVar.take sessionsAVar
  let filteredSessions = sessions # Map.filter (not <<< sessionIsExpired currentTime)
  AVar.put filteredSessions sessionsAVar

verifySession :: AVar Sessions -> ApiUUID -> Aff (Maybe Session)
verifySession sessionsAVar authToken = do
  expireSessions sessionsAVar
  currTime <- unixEpoch
  withAVar sessionsAVar \sessions -> pure
    case Map.lookup authToken sessions of
      Nothing -> Tuple sessions Nothing
      Just (Session session) ->
        let updatedSession = Session $ session { lastTime = currTime }
            newSessions = Map.insert authToken updatedSession sessions
        in Tuple newSessions (Just updatedSession)
    
createSession :: AVar Sessions -> String -> Aff ApiUUID
createSession sessionsAVar userName = do
  lastTime <- unixEpoch
  authToken <- liftEffect genApiUUID
  sessions <- AVar.take sessionsAVar
  let newSession = Session {authToken, userName, lastTime}
  let updatedSessions = Map.insert authToken newSession sessions
  AVar.put updatedSessions sessionsAVar
  pure authToken

deleteSession :: AVar Sessions -> ApiUUID -> Aff Unit
deleteSession sessionsAVar authToken = do
  sessions <- AVar.take sessionsAVar
  AVar.put (Map.delete authToken sessions) sessionsAVar
