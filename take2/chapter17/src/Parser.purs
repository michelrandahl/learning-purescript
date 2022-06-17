module Parser where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Either (Either(..))
import Data.String.CodeUnits (uncons, fromCharArray)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Unfoldable (class Unfoldable, replicate, none)
import Data.Traversable (class Traversable, sequence)

type ParserState a = Tuple String a

class ParseError (e :: Type) where
  eof :: e

type ParseFunction e a = ParseError e => String -> Either e (ParserState a)

newtype Parser e a = Parser (ParseFunction e a)

parse :: forall e a. Parser e a -> ParseFunction e a
parse (Parser parseFun) = parseFun

instance Functor (Parser e) where
  map :: forall a b. (a -> b) -> Parser e a -> Parser e b
  map f p = Parser (map (map f) <<< parse p)

instance Apply (Parser e) where
  apply :: forall a b. Parser e (a -> b) -> Parser e a -> Parser e b
  apply p1 p2 =
    Parser (\s -> case parse p1 s of
        Left err -> Left err
        Right (Tuple s1 f') -> case parse p2 s1 of
          Left err -> Left err
          Right (Tuple s2 a) -> Right $ Tuple s2 (f' a))

instance Applicative (Parser e) where
  pure :: forall a. a -> Parser e a
  pure x = Parser \s -> pure $ Tuple s x

char :: forall e. Parser e Char
char = Parser \s -> case uncons s of
  Nothing -> Left eof
  Just {head,tail} -> Right $ Tuple tail head

data PError = EOF
derive instance Generic PError _
instance Show PError where
  show = genericShow
instance ParseError PError where
  eof = EOF

twoChars :: forall e. Parser e (Tuple Char Char)
twoChars = Tuple <$> char <*> char

count :: forall a e f. Traversable f => Unfoldable f => Int -> Parser e a -> Parser e (f a)
count n p
  | n <= 0 = pure none
  | otherwise = sequence (replicate n p)

threeChars :: forall e. Parser e String
--threeChars = (\c1 c2 c3 -> fromCharArray [c1, c2, c3]) <$> char <*> char <*> char
threeChars = fromCharArray <$> count 3 char

parse' :: forall a. Parser PError a -> ParseFunction PError a
parse' = parse

test :: Effect Unit
test = do
  log $ show $ parse' char "ABC"
  log $ show $ parse' twoChars "ABC"
  log $ show $ parse' threeChars "ABC"
  log $ show $ parse' threeChars "A"

