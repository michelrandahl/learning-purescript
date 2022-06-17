module Section12 where

import Data.List (List)
import Data.List as List
import Data.String.CodeUnits as String -- non unicode-module
import Data.Maybe
import Data.String.CodePoints (CodePoint)
import Data.String as StringUnicode
import Type.Proxy (Proxy(..))

-- functional dependency
-- `collection -> element` specifies that there can only be one element type for collection
class Decapitate collection element | collection -> element where
  decapitate :: collection -> Maybe {head :: element, tail :: collection}

instance Decapitate (List a) a where
  decapitate = List.uncons

instance Decapitate String Char where
  decapitate = String.uncons
-- the `else` resolves the ambiguity for String -> 'one single type'.. however, the case would never be matched for the genericTail function.. so it is fruitless
else instance Decapitate String CodePoint where
  decapitate = StringUnicode.uncons

genericTail
  :: forall collection element
   . Decapitate collection element
  => collection
  -> Maybe collection
genericTail xs = case (decapitate xs :: Maybe {head :: element, tail :: collection}) of
  Just { tail } -> Just tail
  Nothing       -> Nothing

-- proxy element implementation that doesn't rely on the functional dependency
genericTail2
  :: forall collection element
   . Decapitate collection element
  => Proxy element
  -> collection
  -> Maybe collection
genericTail2 _ xs = case (decapitate xs :: Maybe {head :: element, tail :: collection}) of
  Just { tail } -> Just tail
  Nothing       -> Nothing

t :: Maybe String
t = genericTail "abc"

t2 :: Maybe String
t2 = genericTail2 (Proxy :: Proxy Char) "abc"
