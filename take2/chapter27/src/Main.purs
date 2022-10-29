module Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Chalky (chalk, Colour(..), TextStyle(..))
import Data.Maybe (Maybe(..))

testChalk :: Effect Unit
testChalk = log $ chalk Red (Just Bold) "my test string" <> chalk Blue (Just Italic) " and more..."

main :: Effect Unit
main = do
  log "🍝"
  testChalk
