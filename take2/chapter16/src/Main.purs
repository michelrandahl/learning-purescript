module Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Data.List (List(..), (:), fromFoldable)
import Data.Tuple (Tuple(..))
import Data.Foldable (class Foldable, foldl, foldr)
import Data.Int (even)
import Data.Traversable (class Traversable, traverse, sequence)
import Control.Alt (class Alt, alt, (<|>))
import Data.Int.Bits ((.&.))
import Control.Plus

fullName :: String -> String -> String -> String
fullName first middle last = first <> " " <> middle <> " " <> last

errIfMissing :: Maybe String -> String -> Either String String
errIfMissing Nothing err = Left err
errIfMissing (Just s) _ = Right s

fullNameEither :: Maybe String -> Maybe String -> Maybe String -> Either String String
fullNameEither first middle last =
  fullName <$> (first `errIfMissing` "First name must exist")
           <*> (middle `errIfMissing` "Middle name must exist")
           <*> (last `errIfMissing` "Last name must exist")


combineList :: forall a f. Applicative f => List (f a) -> f (List a)
combineList Nil = pure Nil
combineList (x : xs) = Cons <$> x <*> combineList xs

combine :: forall a f l. Applicative f => Foldable l => l (f a) -> f (List a)
combine = combineList <<< fromFoldable

half :: Int -> Maybe Int
half x =
  if even x
  then Just (x `div` 2)
  else Nothing

foo :: Maybe (List Int -> List Int)
foo = map Cons $ Just 1

reverse = foldl (flip (:)) Nil

--instance Traversable List where
--  traverse :: forall a b m. Applicative m => (a -> m b) -> List a -> m (List b)
--  traverse f = foldr (\x acc -> (:) <$> f x <*> acc) (pure Nil)
--
--  sequence :: forall a m. Applicative m => List (m a) -> m (List a)
--  sequence = traverse identity

--instance Alt Maybe where
--  alt Nothing r = r
--  alt l _ = l

--instance Alt (Either a) where
--  alt (Left _) r = r
--  alt l _ = l

assertEven :: Int -> Maybe Int
assertEven v = if v .&. 1 == 0 then Just v else Nothing

assertion :: Maybe Int
assertion = assertEven 1 <|> assertEven 2 <|> assertEven 4

--instance Plus List where
--  empty = Nil

main :: Effect Unit
main = do
  log $ show $ Cons 5 Nil
  log $ show $ combineList $ fromFoldable [Just 1, Just 2]
  -- NOTE: 'Just 42' is still being evaluated despite the shortcircuit
  log $ show $ combineList $ fromFoldable [Just 1, Just 2, Nothing, Just 42]
  -- creating every combination of elements in the lists
  log $ show $ Tuple <$> [10,20] <*> [3,4]
  log $ show $ map half [2, 4, 6]
  log $ show $ combine $ map half [2, 4, 6]
  log $ show $ map half [2, 3, 6]
  log $ show $ combine $ map half [2, 3, 6]
  log $ show $ reverse [1,2,3]
  --------------------
  log $ show $ alt Nothing (Nothing :: Maybe Int)
  log $ show $ alt Nothing (Just 42)
  log $ show $ alt (Left 11) (Right 42)
  --------------------
  log $ show assertion
  log $ show $ (empty :: List Int) -- empty is from `Plus`
  log "🍝"
