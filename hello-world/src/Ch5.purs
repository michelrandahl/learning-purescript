module Ch5 where

import Prelude (max, Unit, (<<<), (<), (<=), (==), negate, (-), (+), show, discard, type (~>), otherwise)
import Effect (Effect)
import Effect.Console (log)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Data.Tuple

singleton :: forall a. a -> List a
singleton x = Cons x Nil

snoc :: forall a. List a -> a -> List a
snoc Nil x         = singleton x
snoc (Cons x xs) y = x : snoc xs y

length :: forall a. List a -> Int
length = loop 0
  where
    loop :: Int -> List a -> Int
    loop acc Nil         = acc
    loop acc (Cons _ xs) = loop (acc + 1) xs

head :: forall a. List a -> Maybe a
head Nil = Nothing
head (x : _) = Just x

tail :: forall a. List a -> Maybe (List a)
tail Nil      = Nothing
tail (_ : xs) = Just xs

last :: forall a. List a -> Maybe a
last Nil       = Nothing
last (x : Nil) = Just x
last (x : xs)  = last xs

init :: forall a. List a -> Maybe (List a)
init Nil       = Nothing
init xs        = Just $ loop xs Nil
  where
    loop :: List a -> List a -> List a
    loop Nil acc       = acc
    loop (y : Nil) acc = acc
    loop (y : ys) acc  = loop ys (y : acc)

null :: forall a. List a -> Boolean
null Nil = true
null _ = false

uncons :: forall a. List a -> Maybe { head :: a, tail :: List a }
uncons Nil      = Nothing
uncons (x : xs) = Just {head: x, tail: xs}

index :: forall a. List a -> Int -> Maybe a
index Nil _       = Nothing
index _ n | n < 0 = Nothing
index (x : _) 0   = Just x
index (_ : xs) n  = index xs (n - 1)

infixl 8 index as !!

findIndex :: forall a. (a -> Boolean) -> List a -> Maybe Int
findIndex _ Nil   = Nothing
findIndex pred xs = loop xs 0
  where
    loop :: List a -> Int -> Maybe Int
    loop Nil _        = Nothing
    loop (x' : xs') acc =
      if pred x'
      then Just acc
      else loop xs' (acc + 1)

findLastIndex :: forall a. (a -> Boolean) -> List a -> Maybe Int
findLastIndex _ Nil = Nothing
findLastIndex pred xs = loop xs Nothing 0
  where
    loop :: List a -> Maybe Int -> Int -> Maybe Int
    loop Nil best _          = best
    loop (x' : xs') best acc =
      if pred x'
      then loop xs' (Just acc) (acc + 1)
      else loop xs' best (acc + 1)

reverse :: List ~> List
reverse xs = loop xs Nil
  where
    loop :: forall a. List a -> List a -> List a
    loop Nil reversed        = reversed
    loop (x' : xs') reversed = loop xs' (x' : reversed)

concat :: forall a. List (List a) -> List a
concat Nil = Nil
concat xss = reverse $ loop xss Nil
  where
    loop :: List (List a) -> List a -> List a
    loop Nil acc               = acc
    loop (Nil : xss') acc      = loop xss' acc
    loop ((y : ys) : xss') acc = loop (ys : xss') (y : acc)

filter :: forall a. (a -> Boolean) -> List a -> List a
filter pred xs = reverse $ loop xs Nil
  where
    loop :: List a -> List a -> List a
    loop Nil acc        = acc
    loop (x' : xs') acc =
      if pred x'
      then loop xs' (x' : acc)
      else loop xs' acc

catMaybes :: forall a. List (Maybe a) -> List a
catMaybes ms = reverse $ loop ms Nil
  where
    loop :: List (Maybe a) -> List a -> List a
    loop Nil acc             = acc
    loop (Nothing : ms') acc = loop ms' acc
    loop (Just x : ms') acc  = loop ms' (x : acc)

range :: Int -> Int -> List Int
range start end = reverse $ loop start Nil
  where
    stepfun :: Int -> Int
    stepfun =
      if start <= end
      then (_ + 1)
      else (_ - 1)
    loop :: Int -> List Int -> List Int
    loop n xs
      | n == end  = (n : xs)
      | otherwise = loop (stepfun n) (n : xs)

take :: forall a. Int -> List a -> List a
take n xs = reverse $ loop (max 0 n) xs Nil
  where
    loop :: Int -> List a -> List a -> List a
    loop _ Nil acc         = acc
    loop 0 _ acc           = acc
    loop n' (x' : xs') acc = loop (n' - 1) xs' (x' : acc)

drop :: forall a. Int -> List a -> List a
drop _ Nil = Nil
drop n xs  = loop (max 0 n) xs
  where
    loop :: Int -> List a -> List a
    loop _ Nil        = Nil
    loop 0 xs'        = xs'
    loop n' (_ : xs') = loop (n' - 1) xs'

takeWhile :: forall a. (a -> Boolean) -> List a -> List a
takeWhile pred = reverse <<< loop Nil
  where
    loop :: List a -> List a -> List a
    loop acc Nil = acc
    loop acc (x : xs)
      | pred x    = loop (x : acc) xs
      | otherwise = acc

dropWhile :: forall a. (a -> Boolean) -> List a -> List a
dropWhile _ Nil = Nil
dropWhile pred l@(x : xs)
  | pred x    = dropWhile pred xs
  | otherwise = l

takeEnd :: forall a. Int -> List a -> List a
takeEnd n xs = drop (max 0 (length xs - n)) xs

takeEnd' :: forall a. Int -> List a -> List a
takeEnd' n = snd <<< loop
  where
    loop Nil = Tuple 0 Nil
    loop (x : xs) =
      let Tuple n' xs' = loop xs in
        if n' < n
        then Tuple (n' + 1) (x : xs')
        else Tuple n' xs'

dropEnd :: forall a. Int -> List a -> List a
dropEnd n = snd <<< loop
  where
    loop Nil = Tuple 0 Nil
    loop (x : xs) =
      let Tuple n' xs' = loop xs in
        if n' < n
        then Tuple (n' + 1) Nil
        else Tuple n' (x : xs')

zip :: forall a b. List a -> List b -> List (Tuple a b)
zip xs ys = reverse $ loop Nil xs ys
  where
    loop :: List (Tuple a b) -> List a -> List b -> List (Tuple a b)
    loop acc Nil _ = acc
    loop acc _ Nil = acc
    loop acc (x : xs') (y : ys') = loop (Tuple x y : acc) xs' ys'

unzip :: forall a b. List (Tuple a b) -> Tuple (List a) (List b)
unzip xs =
  let Tuple as bs = loop (Tuple Nil Nil) xs in
      Tuple (reverse as) (reverse bs)
  where
    loop :: Tuple (List a) (List b) -> List (Tuple a b) -> Tuple (List a) (List b)
    loop acc Nil = acc
    loop (Tuple as bs) ((Tuple x y) : xs') = loop (Tuple (x : as) (y : bs)) xs'

apply :: forall a b. (a -> b) -> a -> b
apply f a = f a

infixr 0 apply as $

flip :: forall a b c. (a -> b -> c) -> b -> a -> c
flip f b a = f a b

applyFlipped :: forall a b. a -> (a -> b) -> b
applyFlipped = flip apply

infixl 1 applyFlipped as #

const :: forall a b. a -> b -> a
const a _ = a

test :: Effect Unit
test = do
  log $ show $ flip const 1 2
  log $
    (flip const 1 2)
    # show
  "xyz" # singleton # show # log
  log $ show $ null Nil
  log $ show $ null ("foo" : Nil)
  log $ show $ snoc (1 : 2 : Nil) 3
  log $ show $ length $ 1 : 2 : 3 : Nil
  log $ show $ head $ 1 : 2 : Nil
  log $ show $ (head Nil :: Maybe Unit)
  log $ show $ init (Nil :: List Unit)
  log $ show $ init $ 1 : Nil
  log $ show $ init $ 1 : 2 : Nil
  log $ show $ init $ 1 : 2 : 3 : Nil
  log $ show $ index (1 : 2 :3 : Nil) (-99)
  log $ show $ index (1 : 2 :3 : Nil) 2
  log $ show $ (1 : 2 : 3 : Nil) !! 1
  log $ show $ reverse (1 : 2 : 3 : Nil)
  log $ show $ concat ((1 : 2 : Nil) : Nil : (9 : 8 : 7 : Nil) : Nil)
  log $ show $ filter (_ < 4) (1 : 2 : 5 : 9 : 3 : Nil)
  log $ show $ catMaybes (Just 1 : Nothing : Just 42 : Nothing : Nothing : Nil)
  log $ show $ range 10 22
  log $ show $ range 3 (-3)
  log $ show $ take 3 (1 : 42 : 22 : 99 : Nil)
  log $ show $ take (-2) (1 : 2 : 3 : 4 : Nil)
  log $ show $ drop 2 (1 : 42 : 22 : 99 : Nil)
  log $ show $ drop 2 (Nil :: List Unit)
  log $ show $ drop (-1) (1 : 42 : 22 : 99 : Nil)
  log $ show $ takeWhile (_ < 4) (1 : 3 : 2 : 42 : 1 : Nil)
  log $ show $ dropWhile (_ < 4) (1 : 3 : 2 : 42 : 1 : Nil)
  log $ show $ takeEnd 3 (1 : 3 : 2 : 42 : 1 : Nil)
  log $ show $ takeEnd' 3 (1 : 3 : 2 : 42 : 1 : Nil)
  log $ show $ dropEnd 2 (1 : 3 : 2 : 42 : 1 : Nil)
  log $ show $ dropEnd 10 (1 : 3 : 2 : 42 : 1 : Nil)
  log $ show $ dropEnd 0 (1 : 3 : 2 : 42 : 1 : Nil)
  log $ show $ zip (1 : 2 : 3 : Nil) ("a" : "b" : "c" : Nil)
  log $ show $ unzip $ zip (1 : 2 : 3 : Nil) ("a" : "b" : "c" : Nil)
