module Ch7b where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Data.Newtype (class Newtype)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), split)
import Data.Int (fromString)

newtype CSV = CSV String
derive instance Newtype CSV _
derive newtype instance Eq CSV
instance Show CSV where
  show (CSV content) = content

class ToCSV a where
  toCsv :: a -> CSV

class FromCSV a where
  fromCSV :: CSV -> Maybe a

newtype FullName = FullName String
derive instance Newtype FullName _
derive newtype instance Eq FullName
instance Show FullName where
  show (FullName name) = name

newtype Age = Age Int
derive instance Newtype Age _ -- NOTE: not needed to derive show
derive newtype instance Show Age
derive newtype instance Eq Age

data Occupation = Doctor | Dentist | Lawyer | Unemployed
derive instance Generic Occupation _
derive instance Eq Occupation
instance Show Occupation where
  show = genericShow

toOccupation :: String -> Maybe Occupation
toOccupation "Doctor"     = Just Doctor
toOccupation "Dentist"    = Just Dentist
toOccupation "Lawyer"     = Just Lawyer
toOccupation "Unemployed" = Just Unemployed
toOccupation _            = Nothing

data Person = Person
  { name       :: FullName
  , age        :: Age
  , occupation :: Occupation}
derive instance Eq Person
derive instance Generic Person _
instance Show Person where
  show = genericShow

instance ToCSV Person where
  toCsv (Person {name,age,occupation}) =
    CSV $ show name <> "," <> show age <> "," <> show occupation

instance FromCSV Person where
  fromCSV (CSV str) =
    case split (Pattern ",") str of
      [name, age, occupation] ->
        case fromString age, toOccupation occupation of
          Just age', Just occupation' ->
            Just $ Person { name       : FullName name
                          , age        : Age age'
                          , occupation : occupation'}
          _, _ -> Nothing
      _ -> Nothing

joe :: Person
joe = Person { name       : FullName "Joe"
             , age        : Age 42
             , occupation : Doctor}

csvJoe :: CSV
csvJoe = toCsv joe

joe' :: Maybe Person
joe' = fromCSV csvJoe

test :: Effect Unit
test = do
  log $ show csvJoe
  log $ show joe'
