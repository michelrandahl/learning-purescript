module Entity.User where

import Prelude
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Argonaut.Encode.Class (class EncodeJson)
import Data.Argonaut.Decode.Class (class DecodeJson)

type UserRow r =
  ( userName          :: String
  , temporaryPassword :: Boolean
  , admin             :: Boolean
  , firstName         :: String
  , lastName          :: String
  | r
  )
newtype User = User (Record (UserRow ()))

derive instance Generic User _
instance Show User where
  show = genericShow
derive newtype instance EncodeJson User
derive newtype instance DecodeJson User
