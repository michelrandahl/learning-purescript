module Ffi1 where

import Data.Maybe (Maybe(..))

foreign import myfunImpl :: forall a b. (a -> Maybe b) -> Maybe b -> Int -> Maybe Int

myfun :: Int -> Maybe Int
myfun x = myfunImpl Just Nothing x
