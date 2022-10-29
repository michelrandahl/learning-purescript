module Main where

import Prelude

import Control.Monad.Error.Class (try)
import Data.Either (Either(..))
import Data.String.Common (toUpper, toLower)
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff, Fiber, Canceler(..), launchAff_, delay, killFiber, forkAff, runAff, cancelWith)
import Effect.Aff.AVar (AVar)
import Effect.Aff.AVar as AVar
import Effect.Aff.Bus (BusR, BusW)
import Effect.Aff.Bus as Bus
import Effect.Class.Console (log)
import Effect.Exception (Error, error, message)
import Node.Encoding (Encoding(..))
import Node.FS.Aff as FSAff
import Node.FS.Async as FSAsync

displayFile :: Either Error String -> Effect Unit
displayFile (Right fileData) = log fileData
displayFile (Left err) = log $ show err

readAFile :: String -> Aff Unit
readAFile fileName = do
  text <- FSAff.readTextFile ASCII fileName
  log text

readAFile2 :: String -> Aff Unit
readAFile2 fileName = do
  result <- try $ FSAff.readTextFile ASCII fileName
  case result of
    Right fileData -> log fileData
    Left err -> log $ show err

logEverySecond :: Aff Unit
logEverySecond = go 0 where
  go x = do
    log $ show x
    delay (Milliseconds 1000.0)
    go $ x + 1

readAFileAfterTwoSeconds :: String -> Aff String
readAFileAfterTwoSeconds fileName = do
  delay (Milliseconds 2000.0)
  result <- try $ FSAff.readTextFile ASCII fileName
  pure $ case result of
    Right text -> text
    Left err -> show err

kill :: forall a. Fiber a -> Aff Unit
kill = killFiber (error "killing you softly..")

testFiber :: String -> Effect Unit
testFiber fileName = do
  logger <- runAff (const $ pure unit)
    $ (cancelWith logEverySecond (Canceler (log <<< message)))
  fileReader <- runAff case _ of -- NOTE that this is the purescript way of creating a lambda
    Left err -> log $ show err
    Right result -> log $ "file contents: " <> show result
    $ readAFileAfterTwoSeconds fileName
  launchAff_ do
    delay (Milliseconds 5000.0)
    kill logger
    kill fileReader

readAFileAfterTwoSeconds2 :: AVar String -> Aff Unit
readAFileAfterTwoSeconds2 fileAVar = do
  delay (Milliseconds 2000.0)
  result <- try $ FSAff.readTextFile ASCII "test.txt"
  case result of
    Right text -> AVar.put ("inside avar: " <> text) fileAVar
    Left err -> log $ show err

processFile :: AVar String -> Aff Unit
processFile fileAVar = do
  text <- AVar.take fileAVar
  log text

testAVar :: Effect Unit
testAVar = launchAff_ do
  fileAVar <- AVar.empty
  void $ forkAff $ processFile fileAVar
  void $ forkAff $ readAFileAfterTwoSeconds2 fileAVar

readAFile3 :: BusW String -> Aff Unit
readAFile3 fileBus = do
  result <- try $ FSAff.readTextFile ASCII "test.txt"
  case result of
    Right text -> Bus.write text fileBus
    Left err -> log $ show err

processFile2 :: (String -> String) -> BusR String -> Aff Unit
processFile2 convert fileBus = do
  text <- Bus.read fileBus
  log $ convert text

testBus :: Effect Unit
testBus = launchAff_ do
  fileBus <- Bus.make
  let (Tuple readBus writeBus) = Bus.split fileBus
  void $ forkAff $ processFile2 (("IN THE BUS: " <> _) <<< toUpper) readBus
  void $ forkAff $ processFile2 (("in the bus: " <> _) <<< toLower) readBus
  void $ forkAff $ readAFile3 writeBus

main :: Effect Unit
main = do
  FSAsync.readTextFile ASCII "test.txt" displayFile
  FSAsync.readTextFile ASCII "foobar1.txt" displayFile
  launchAff_ $ readAFile "test.txt"
  --throws exception and crashes program
  --launchAff_ $ readAFile "foobar.txt"
  --but this one will handle the exception
  launchAff_ $ readAFile2 "foobar2.txt"
  launchAff_ $ readAFile2 "test.txt"
  testFiber "test.txt"
  testFiber "foobar3.txt"
  testAVar
  testBus
