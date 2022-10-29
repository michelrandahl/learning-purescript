module Common.ApiUUID where

import Data.Argonaut.Decode.Class (class DecodeJson, decodeJson)
import Data.Argonaut.Decode.Error (JsonDecodeError(..))
import Data.Argonaut.Encode.Class (class EncodeJson, encodeJson)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.UUID (UUID, emptyUUID, parseUUID, toString, genUUID)
import Effect (Effect)
import Prelude

newtype ApiUUID = ApiUUID UUID

emptyApiUUID :: ApiUUID
emptyApiUUID = ApiUUID emptyUUID

genApiUUID :: Effect ApiUUID
genApiUUID = ApiUUID <$> genUUID

derive instance Generic ApiUUID _
instance Show ApiUUID where
  show (ApiUUID uuid) = toString uuid
instance EncodeJson ApiUUID where
  encodeJson = encodeJson <<< show
instance DecodeJson ApiUUID where
  decodeJson json = do
    decoded :: String <- decodeJson json
    case parseUUID decoded of
      Nothing     -> Left $ TypeMismatch "not uuid"
      Just parsed -> Right $ ApiUUID parsed
derive newtype instance Eq ApiUUID
derive newtype instance Ord ApiUUID
