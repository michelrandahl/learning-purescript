module Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Data.Tuple (Tuple(..))
import Data.Int.Bits ((.&.))
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)

--type Debuggable a = Tuple String a

--type Countable a = Tuple Int a

--class Applicative f <= SideEffect f where
--  applySideEffect :: forall a b. f a -> (a -> f b) -> f b

newtype Debuggable a = Debuggable (Tuple String a)

derive newtype instance Functor Debuggable
derive newtype instance Apply Debuggable
derive newtype instance Applicative Debuggable

instance Bind Debuggable where
  bind :: forall a b. Debuggable a -> (a -> Debuggable b) -> Debuggable b
  bind (Debuggable (Tuple s x)) f =
    let (Debuggable (Tuple s' r)) = f x in
        Debuggable $ Tuple (s <> s') r
  -- Degenerate version of bind
  --bind (Debuggable (Tuple _ x)) f = f x

--instance SideEffect Debuggable where
--  applySideEffect (Debuggable (Tuple s x)) f =
--    let Debuggable (Tuple s' r) = f x in
--        Debuggable $ Tuple (s <> s') r

newtype Count = Count Int
derive newtype instance Semiring Count
instance Semigroup Count where
  append = (+)
instance Monoid Count where
  mempty = zero

newtype Countable a = Countable (Tuple Count a)

derive newtype instance Functor Countable
derive newtype instance Apply Countable
derive newtype instance Applicative Countable

--instance SideEffect Countable where
--  applySideEffect (Countable (Tuple c x)) f =
--    let Countable (Tuple c' r) = f x in
--        Countable $ Tuple (c + c') r

composeKleisli :: forall a b c m. Monad m => (b -> m c) -> (a -> m b) -> a -> m c
composeKleisli g f x = f x >>= g

infixr 5 composeKleisli as >=>

--newtype Kleisli :: forall k. (k -> Type) -> Type -> k -> Type
newtype Kleisli m a b = Kleisli (a -> m b)

instance Monad m => Semigroupoid (Kleisli m) where
  compose (Kleisli g) (Kleisli f) = Kleisli \x -> f x >>= g
  -- following doesnt work for mysterious reasons...
  --compose (Kleisli g) (Kleisli f) = Kleisli $ f >=> g

instance Monad m => Category (Kleisli m) where
  --identity :: forall a. a -> a
  --identity :: forall a. (->) a a
  --identity :: forall a. (Kleisli m) a a
  identity :: forall a. Kleisli m a a
  identity = Kleisli pure

f1 :: Int -> Debuggable Int
f1 x = Debuggable $ Tuple "added 10\n" (x + 10)

g1 :: Int -> Debuggable Int
g1 x = Debuggable $ Tuple "added 100\n" (x + 100)

h :: Int -> Debuggable Int
h = g1 `composeDebuggable` f1

applyDebuggable :: forall a b. Debuggable a -> (a -> Debuggable b) -> Debuggable b
applyDebuggable (Debuggable (Tuple s x)) f =
  let Debuggable (Tuple s' r) = f x in
      Debuggable $ Tuple (s <> s') r

composeDebuggable :: forall b c d. (c -> Debuggable d) -> (b -> Debuggable c) -> b -> Debuggable d
composeDebuggable g f x = f x `applyDebuggable` g

noSideEffect :: Int -> Int
noSideEffect = (+) 42

--c :: Int -> Debuggable Int
--c = makeFuncDebuggable noSideEffect `composeDebuggable` f1

--makeDebuggable :: forall a. a -> Debuggable a
--makeDebuggable x = Tuple "" x

--makeFuncDebuggable :: forall a b. (a -> b) -> a -> Debuggable b
--makeFuncDebuggable f x = makeDebuggable (f x)
--
--y :: Debuggable Int
--y = makeDebuggable 12345 `applyDebuggable` f1 `applyDebuggable` g1

data Maybe a = Nothing | Just a
derive instance Generic (Maybe a) _
instance Show a => Show (Maybe a) where
  show = genericShow

instance Functor Maybe where
  map _ Nothing = Nothing
  map f (Just x) = Just $ f x

instance Applicative Maybe where
  pure = Just

instance Bind Maybe where
  bind :: forall a b. Maybe a -> (a -> Maybe b) -> Maybe b
  bind (Just x) f = f x
  bind Nothing  _ = Nothing

ap :: forall a b m. Monad m => m (a -> b) -> m a -> m b
ap mf mx = do
  f <- mf
  x <- mx
  pure $ f x
  
instance Apply Maybe where
  apply = ap

instance Monad Maybe

--oddTest :: Int -> Maybe Int
--oddTest x = if x .&. 1 == 1 then Just x else Nothing
--
--greaterThanTest :: Int -> Int -> Maybe Int
--greaterThanTest min x = if x > min then Just x else Nothing
--
--lessThanTest :: Int -> Int -> Maybe Int
--lessThanTest max x = if x < max then Just x else Nothing
--
--gauntlet :: Int -> Maybe Int
--gauntlet = oddTest >=> pure <<< (_ + 1) >=> greaterThanTest 10 >=> lessThanTest 20
--
--gauntlet2 :: Int -> Maybe Int
--gauntlet2 x = do
--  o <- oddTest x
--  let y = o + 1
--  --z <- greaterThanTest 10 y
--  --lessThanTest 20 z
--  -- note that the void line still has the effect of short circuiting the flow and returning `Nothing`
--  void $ greaterThanTest 10 y
--  lessThanTest 20 y

data Either a b = Left a | Right b
derive instance Generic (Either a b) _
instance (Show a, Show b) => Show (Either a b) where
  show = genericShow

instance Functor (Either a) where
  map _ (Left err) = Left err
  map f (Right x) = Right $ f x

instance Bind (Either l) where
  bind :: forall a b. Either l a -> (a -> Either l b) -> Either l b
  bind (Left l) _ = Left l
  bind (Right r) f = f r

instance Apply (Either a) where
  apply = ap

instance Applicative (Either a) where
  pure = Right

instance Monad (Either a)

oddTest :: Int -> Either String Int
oddTest x = if x .&. 1 == 1 then Right x else Left "Number is not odd"

greaterThanTest :: Int -> Int -> Either String Int
greaterThanTest min x =
  if x > min then Right x
  else Left $ "Number is not greater than " <> show min

lessThanTest :: Int -> Int -> Either String Int
lessThanTest max x =
  if x < max then Right x
  else Left $ "Number is not less than " <> show max

gauntlet :: Int -> Either String Int
gauntlet x = do
  o <- oddTest x
  let y = o + 1
  void $ greaterThanTest 10 y
  lessThanTest 20 y

errIfMissing :: Maybe String -> String -> Either String String
errIfMissing Nothing err = Left err
errIfMissing (Just s) _ = Right s

fullName :: String -> String -> String -> String
fullName first middle last = first <> " " <> middle <> " " <> last

fullNameEither :: Maybe String -> Maybe String -> Maybe String -> Either String String
fullNameEither first middle last = do
  f <- errIfMissing first "first name must exist"
  m <- errIfMissing middle "middle name must exist"
  l <- errIfMissing last "last name must exist"
  pure $ fullName f m l

newtype Validation err result = Validation (Either err result)
derive newtype instance Functor (Validation err)

instance Semigroup err => Apply (Validation err) where
  apply (Validation (Left err1)) (Validation (Left err2)) = Validation $ Left (err1 <> err2)
  apply (Validation (Left err)) _ = Validation $ Left err
  apply (Validation (Right f)) x = f <$> x

-- Bind is no good for `Validation` since we cannot collect errors
--instance Semigroup err => Bind (Validation err) where
--  bind :: forall a b. (Validation err a) -> (a -> Validation err b) -> Validation err b
--  bind (Validation (Left err1)) _ = Validation $ Left err1
--  bind (Validation (Right r)) f = f r

newtype Writer w a = Writer (Tuple a w)
instance Functor (Writer w) where
  map :: forall a b. (a -> b) -> Writer w a -> Writer w b
  map f (Writer (Tuple a w)) = Writer $ Tuple (f a) w

instance Monoid w => Apply (Writer w) where
  apply :: forall a b. Writer w (a -> b) -> Writer w a -> Writer w b
  apply = ap

instance Monoid w => Applicative (Writer w) where
  pure x = Writer $ Tuple x mempty

instance Monoid w => Bind (Writer w) where
  bind :: forall a b. Writer w a -> (a -> Writer w b) -> Writer w b
  bind (Writer (Tuple a w1)) f =
    let Writer (Tuple b w2) = f a in
      Writer $ Tuple b (w1 <> w2)

instance Monoid w => Monad (Writer w)

tell :: forall w. w -> Writer w Unit
tell l = Writer $ Tuple unit l

runWriter :: forall a w. Writer w a -> Tuple a w
runWriter (Writer x) = x

doNothingWithLog :: Writer (Array String) Int
doNothingWithLog = do
  tell ["We did nothing"]
  pure 0

newtype Reader r a = Reader (r -> a)

instance Functor (Reader r) where
  map f (Reader g) = Reader $ f <<< g

instance Apply (Reader r) where
  apply (Reader ff) (Reader fx) =
    Reader \r -> ff r $ fx r

instance Applicative (Reader r) where
  pure = Reader <<< const

runReader :: forall a r. Reader r a -> r -> a
runReader (Reader f) = f

instance Bind (Reader r) where
  bind (Reader g) f =
    Reader \r -> runReader (f $ g r) r

instance Monad (Reader r)

ask :: forall r. Reader r r
ask = Reader identity

asks :: forall a r. (r -> a) -> Reader r a
asks = Reader

newtype State s a = State (s -> Tuple a s)

instance Functor (State s) where
  map f (State fx) =
    State \s ->
      fx s
      # \(Tuple x s') -> Tuple (f x) s'

instance Apply (State s) where
  apply (State ff) (State fx) =
    State \s ->
      ff s
      # \(Tuple f s') -> fx s'
      # \(Tuple x s'') -> Tuple (f x) s''

instance Applicative (State s) where
  pure x = State \s -> Tuple x s

runState :: forall a s. State s a -> s -> Tuple a s
runState (State f) = f

instance Bind (State s) where
  bind (State fx) f =
    State \s ->
      fx s
      # \(Tuple x s') -> runState (f x) s'

instance Monad (State s)

get :: forall s. State s s
get = State \s -> Tuple s s

put :: forall s. s -> State s Unit
put = State <<< const <<< Tuple unit 

modify :: forall s. (s -> s) -> State s s
modify f =
  State \s ->
    f s
    # \s' -> Tuple s' s'

modify_ :: forall s. (s -> s) -> State s Unit
modify_ f =
  State \s ->
    f s
    # Tuple unit

errIfMissing2 :: Maybe String -> String -> Writer (Array String) String
errIfMissing2 Nothing err = do
  tell [err]
  pure mempty
errIfMissing2 (Just s) _ = pure s

fullNameValid :: Maybe String -> Maybe String -> Maybe String
                 -> Writer (Array String) String
fullNameValid first middle last = do
  f <- errIfMissing2 first "first name must exist"
  m <- errIfMissing2 middle "middle name must exist"
  l <- errIfMissing2 last "last name must exist"
  pure $ fullName f m l

main :: Effect Unit
main = do
  log $ show $ gauntlet 14
  log $ show $ gauntlet 1
  log $ show $ gauntlet 93
  log $ show $ gauntlet 17
  log $ show $ runWriter (fullNameValid Nothing (Just "") Nothing)
