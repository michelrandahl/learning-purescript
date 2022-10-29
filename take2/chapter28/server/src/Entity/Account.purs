module Entity.Account where

import Entity.User (UserRow)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Prelude

newtype Account = Account (Record (UserRow ( passwordHash :: String)))
derive instance Generic Account _
instance Show Account where
  show = genericShow
