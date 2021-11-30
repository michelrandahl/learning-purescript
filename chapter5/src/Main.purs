module Main where

import Prelude (Unit, show, discard)

import Effect (Effect)
import Effect.Console (log)

const :: forall a b. a -> b -> a
const x _ = x

flip :: forall a b c. (a -> b -> c) -> b -> a -> c
flip f x y = f y x

apply :: forall a b. (a -> b) -> a -> b
apply f = f

infixr 0 apply as $

main :: Effect Unit
main = do
  log (show (flip const 1 2))
  log $ show $ flip const 1 2
