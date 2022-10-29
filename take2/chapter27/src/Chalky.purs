module Chalky where

import Data.Function.Uncurried (Fn2, runFn2)
import Prelude
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.String.Common (toLower)
import Data.Maybe (Maybe(..))

type Style = String

foreign import _chalk :: Fn2 (Array Style) String String

data Colour = Red | Blue | Green
derive instance Generic Colour _
instance Show Colour where
  show = toLower <<< genericShow

data TextStyle = Bold | Italic | Dim | StrikeThrough
derive instance Generic TextStyle _
instance Show TextStyle where
  show = toLower <<< genericShow

chalk :: Colour -> Maybe TextStyle -> String -> String
chalk colour Nothing str = runFn2 _chalk [show colour] str
chalk colour (Just textStyle) str = runFn2 _chalk [show colour, show textStyle] str

