module Main where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Data.Maybe (Maybe(..))
import Ch5 as Ch5

isNothing :: forall a . Maybe a -> Boolean
isNothing Nothing = true
isNothing _ = false

myfun :: (Int -> Boolean) -> Int -> String
myfun pred x =
  if pred x
  then "foobar"
  else "stuff"

type Person =
  { name :: String
  , age  :: Int
  }

createPerson :: String -> Int -> Person
createPerson = {name: _, age: _}

make18 :: Person -> Person
make18 = _ { age = 18 }

main :: Effect Unit
main = do log "hello world!"
          log (show (isNothing $ Just 42))
          log (myfun (_ == 42) 13)
          Ch5.test
