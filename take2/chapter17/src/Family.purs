module Family

( Age (..)
, FamilyAges(..)
, FamilyAgesRow(..)
, Validation(..)
, createFamilyAges
) where

import Prelude
import Data.Newtype (class Newtype)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Bifunctor (class Bifunctor)

newtype Validation err result = Validation (Either err result)
derive instance Newtype (Validation err result) _
derive newtype instance Functor (Validation err)
derive newtype instance Bifunctor Validation
derive instance (Eq err, Eq res) => Eq (Validation err res)
derive instance (Ord err, Ord res) => Ord (Validation err res)

instance Semigroup err => Apply (Validation err) where
  apply (Validation (Left err1)) (Validation (Left err2)) =
    Validation $ Left (err1 <> err2)
  apply (Validation (Right f)) x  = map f x
  apply (Validation (Left err)) _ = Validation $ Left err

instance Semigroup err => Applicative (Validation err) where
  pure = Validation <<< Right

derive instance Generic (Validation err res) _
instance (Show err, Show res) => Show (Validation err res) where
  show = genericShow


newtype Age = Age Int
derive instance Generic Age _
instance Show Age where
  show = genericShow

newtype FullName = FullName String
derive instance Generic FullName _
instance Show FullName where
  show = genericShow

type FamilyAgesRow r  = ( fatherAge :: Age, motherAge :: Age, childAge :: Age | r )
type FamilyNamesRow r = ( fatherName :: FullName, motherName :: FullName, childName :: FullName | r )

newtype Family = Family { | FamilyNamesRow (FamilyAgesRow ()) }
derive instance Generic Family _
instance Show Family where
  show = genericShow

newtype FamilyAges = FamilyAges { | FamilyAgesRow () }
derive instance Generic FamilyAges _
instance Show FamilyAges where
  show = genericShow

newtype UpperAge = UpperAge Int
newtype LowerAge = LowerAge Int

validateAge :: LowerAge -> UpperAge -> Age -> String -> Validation (Array String) Age
validateAge (LowerAge l) (UpperAge u) (Age a) who
  | a > u = Validation $ Left $ pure (who <> " is too old")
  | a < l = Validation $ Left $ pure (who <> " is too young")
  | otherwise = Validation $ Right $ Age a

createFamilyAges :: { | FamilyAgesRow () } -> Validation (Array String) FamilyAges
createFamilyAges {fatherAge, motherAge, childAge} =
  FamilyAges
    <$> ({ fatherAge: _, motherAge: _, childAge: _ }
        <$> (validateAge (LowerAge 18) (UpperAge 120) fatherAge "Father")
        <*> (validateAge (LowerAge 18) (UpperAge 120) motherAge "Mother")
        <*> (validateAge (LowerAge 0) (UpperAge 18) childAge "Child"))
    
  


