module Entity.Session where

import Common.ApiUUID (ApiUUID)

newtype Session = Session
  { authToken :: ApiUUID
  , userName  :: String
  , lastTime  :: Number
  }
