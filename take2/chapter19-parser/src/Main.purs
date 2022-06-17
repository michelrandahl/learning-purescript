module Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Data.Either (Either(..))
import Data.Tuple (Tuple(..))
import Data.String.CodeUnits (uncons, fromCharArray, singleton)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.CodePoint.Unicode (isDecDigit, isAlpha)
import Data.String.CodePoints (codePointFromChar)
import Control.Alt (class Alt, (<|>))
import Data.Unfoldable (replicate, class Unfoldable, none)
import Data.Traversable (class Traversable, sequence)
import PointFree ((<..))
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Array ((:))
import Data.Int (fromString)
import Control.Lazy (class Lazy, defer)
import Data.NonEmpty (NonEmpty, (:|), fromNonEmpty)
--a function which cause an exception to be thrown
--import Partial.Unsafe (unsafeCrashWith)

type ParserState a = Tuple String a

class ParserError e where
  eof :: e
  invalidChar :: String -> e

type ParseFunction e a =
  ParserError e => String -> Either e (ParserState a)

newtype Parser e a = Parser (ParseFunction e a)

parse :: forall e a. Parser e a -> ParseFunction e a
parse (Parser f) = f

instance Functor (Parser e) where
  map :: forall a b. (a -> b) -> Parser e a -> Parser e b
  map f p = Parser \s -> map f <$> parse p s

instance Alt (Parser e) where
  alt :: forall a. Parser e a -> Parser e a -> Parser e a
  alt a b =
    Parser \s ->
      case parse a s of
        Right x -> Right x
        Left _ -> parse b s

instance Applicative (Parser e) where
  pure :: forall a. a -> Parser e a
  pure x = Parser \s -> pure (Tuple s x)

instance Bind (Parser e) where
  bind :: forall a b. Parser e a → (a → Parser e b) → Parser e b
  bind p f =
    Parser \s -> do
      Tuple s1 x <- parse p s
      parse (f x) s1

instance Monad (Parser e)

instance Apply (Parser e) where
  apply :: forall a b. Parser e (a -> b) -> Parser e a -> Parser e b
  apply = ap

instance Lazy (Parser e a) where
  defer :: (Unit -> Parser e a) -> Parser e a
  defer f = Parser \s -> parse (f unit) s

char :: forall e. Parser e Char
char = Parser \s ->
  case uncons s of
    Nothing -> Left eof
    Just { head, tail } -> Right $ Tuple tail head


-- applicative version of `twoChars`
twoCharsA :: forall e. Parser e (Tuple Char Char)
twoCharsA = Tuple <$> char <*> char

twoChars :: forall e. Parser e (Tuple Char Char)
twoChars = do
  x <- char
  y <- char
  pure $ Tuple x y

threeCharsA :: forall e. Parser e String
threeCharsA = (\c1 c2 c3 -> fromCharArray [c1, c2, c3]) <$> char <*> char <*> char

threeChars :: forall e. Parser e String
threeChars = do
  c1 <- char
  c2 <- char
  c3 <- char
  pure $ fromCharArray [c1,c2,c3]

data PError
  = EOF
  | InvalidChar String

instance ParserError PError where
  eof         = EOF
  invalidChar = InvalidChar

derive instance Generic PError _
instance Show PError where
  show = genericShow

fail :: forall e a. ParserError e => e -> Parser e a
fail err = Parser $ const (Left err)

satisfy :: forall e. ParserError e => String -> (Char -> Boolean) -> Parser e Char
satisfy expected pred = do
  x <- char
  if pred x
  then pure x
  else fail $ invalidChar expected

digit :: forall e. ParserError e => Parser e Char
digit = satisfy "digit" (isDecDigit <<< codePointFromChar)

letter :: forall e. ParserError e => Parser e Char
letter = satisfy "letter" (isAlpha <<< codePointFromChar)

alphaNum :: forall e. ParserError e => Parser e Char
alphaNum = letter <|> digit <|> fail (invalidChar "alphaNum")

optional :: forall e a. a -> Parser e a -> Parser e a
optional x p = p <|> pure x

atMost :: forall e a f.
          Unfoldable f =>
          (a -> f a -> f a) -> Int -> Parser e a -> Parser e (f a)
atMost cons n p
  | n <= 0    = pure none
  | otherwise =
    (do x <- p
        xs <- atMost cons (n - 1) p
        pure $ cons x xs)
    # optional none

count :: forall e a f.
         Traversable f => Unfoldable f =>
         Int -> Parser e a -> Parser e (f a)
count n p
  | n <= 0    = pure none
  | otherwise = sequence (replicate n p)

range :: forall e a f.
         Traversable f => Unfoldable f => Semigroup (f a) =>
         (a -> f a -> f a) -> Int -> Int -> Parser e a -> Parser e (f a)
range cons min max p
  | min < 0 || max <= 0 || max < min = pure none
  | otherwise = do
    xs :: f a <- count min p
    ys :: f a <- atMost cons (max - min) p
    pure $ xs <> ys

-- zero or more
many :: forall a f m.
        Alt m =>
        Applicative m =>
        Lazy (m (f a)) =>
        Unfoldable f =>
        (a -> f a -> f a) -> m a -> m (f a)
many cons p = fromNonEmpty cons <$> (some cons p) <|> pure none

-- one or more
-- NOTE using lazy defer of the evaluation of the many parser, this will avoid stackoverflow
some :: forall a f m.
        Lazy (m (f a)) =>
        Alt m =>
        Applicative m =>
        Unfoldable f =>
        (a -> f a -> f a) -> m a -> m (NonEmpty f a)
some cons p = (:|) <$> p <*> defer \_ -> many cons p

-------------------- concrete helpers --------------------

range' :: forall e. Int -> Int -> Parser e Char -> Parser e String
range' min max p = fromCharArray <$> range (:) min max p

atMost' :: forall e. Int -> Parser e Char -> Parser e String
atMost' n p = fromCharArray <$> atMost (:) n p

-- NOTE `<<<` takes a function with only one arg
-- to use a function with more args, use compose functions from `PointFree` library, eg `<..`
count' :: forall e. Int -> Parser e Char -> Parser e String
count' = map fromCharArray <.. count

parse' :: forall a. Parser PError a -> ParseFunction PError a
parse' = parse

newtype Year = Year Int
derive instance Generic Year _
instance Show Year where
  show = genericShow

newtype Month = Month Int
derive instance Generic Month _
instance Show Month where
  show = genericShow

newtype Day = Day Int
derive instance Generic Day _
instance Show Day where
  show = genericShow

data DateFormat = YearFist | MonthFirst
derive instance Generic DateFormat _
instance Show DateFormat where
  show = genericShow

type DateParts =
  { day    :: Day
  , month  :: Month
  , year   :: Year
  , format :: DateFormat
  }

dash :: forall e. ParserError e => Parser e Char
dash = satisfy "dash" (_ == '-')

space :: forall e. ParserError e => Parser e Char
space = satisfy "space" (_ == ' ')

constChar' :: forall e. ParserError e => Char -> Parser e Char
constChar' c = satisfy (singleton c) (_ == c)

constChar :: forall e. ParserError e => Char -> Parser e Unit
constChar = void <<< constChar'

digitsToNum :: String -> Int
digitsToNum = fromMaybe 0 <<< fromString

yearFist :: forall e. ParserError e => Parser e DateParts
yearFist = do
  year <- (Year <<< digitsToNum) <$> count' 4 digit
  constChar '-'
  month <- (Month <<< digitsToNum) <$> range' 1 2 digit
  constChar '-'
  day <- (Day <<< digitsToNum) <$> range' 1 2 digit
  pure { format : YearFist, day, month, year }

monthFirst :: forall e. ParserError e => Parser e DateParts
monthFirst = do
  month <- (Month <<< digitsToNum) <$> range' 1 2 digit
  constChar '/'
  day <- (Day <<< digitsToNum) <$> range' 1 2 digit
  constChar '/'
  year <- (Year <<< digitsToNum) <$> count' 4 digit
  pure { format : MonthFirst, day, month, year }

date :: forall e. ParserError e => Parser e DateParts
date = yearFist <|> monthFirst

some' :: forall e. Parser e Char -> Parser e String
some' p = fromCharArray <<< fromNonEmpty (:) <$> some (:) p

many' :: forall e. Parser e Char -> Parser e String
many' p = fromCharArray <$> many (:) p

parseNShow :: forall a. Show a => Parser PError a -> String -> Effect Unit
parseNShow p = log <<< show <<< parse' p

digits :: forall e. ParserError e => Parser e String
digits = some' digit

myParser :: forall e. ParserError e => Parser e (Array String)
myParser = do
  first <- range' 1 4 digit
  constChar ','
  constChar ' '
  second <- some' (letter <|> constChar' ' ')
  third <- many' digit
  pure [first, second, third]

-- applicative style
myParser2 :: forall e. ParserError e => Parser e (Array String)
myParser2 =
  (\s1 s2 s3 -> [s1, s2, s3])
    <$> range' 1 4 digit
    <* constChar ','
    <* constChar ' '
    <*> some' (letter <|> constChar' ' ')
    <*> many' digit

-- following will just cause the whole program to crash immediatly, cause purescript is not a lazy language
--test :: Int
--test = Just 10 # fromMaybe (unsafeCrashWith "got a nothing")

main :: Effect Unit
main = do
  parseNShow (count' 2 digit) "12"
  parseNShow (count' 3 digit) "123456"
  parseNShow (count' 3 digit) "abc456"
  parseNShow (count' 4 letter) "freddy"
  parseNShow (count' 10 alphaNum) "a1b2c3d4e5"
  parseNShow (count' 10 alphaNum) "##########"
  -------------------- atMost --------------------
  parseNShow (atMost' (-2) alphaNum) "a1b2c3"
  parseNShow (atMost' 2 alphaNum) "$_$42"
  parseNShow (atMost' 2 alphaNum) "1"
  parseNShow (atMost' 2 alphaNum) "1#"
  parseNShow (atMost' 1 alphaNum) ""
  -------------------- range --------------------
  parseNShow (range' 0 3 alphaNum) "123"
  parseNShow (range' 2 3 alphaNum) "123"
  parseNShow (range' 2 3 alphaNum) "23"
  -------------------- DateParts --------------------
  parseNShow yearFist "2022-5-1"
  parseNShow monthFirst "5/22/2022"
  -------------------- many and some --------------------
  parseNShow (some' digit) "123"
  parseNShow (many' digit) "123"
  parseNShow (many' digit) "abc"
  parseNShow (some' digit) "abc"
  -------------------- my parser --------------------
  parseNShow myParser "17, some words"
  parseNShow myParser "5432, some more words134567980"
  parseNShow myParser "not a match"

