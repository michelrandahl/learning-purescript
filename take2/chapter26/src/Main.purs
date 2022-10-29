module Main where

import Prelude

import Effect (Effect)
import Effect.Class.Console (log)
import NodePath (joinPath, joinPath2, resolve, readFile)
import Ffi1 (myfun)
import Effect.Aff (launchAff_)

main :: Effect Unit
main = do
  log "🍝"
  log $ show $ joinPath "abc" "xyz.txt"
  log $ show $ joinPath2 "abc" "xyz.txt"
  log $ show $ myfun 11
  log $ show $ myfun 43
  mypath <- resolve ["/foo", "/bar"] "baz"
  log mypath
  launchAff_ do
    contents <- readFile "src/Main.purs"
    log contents
