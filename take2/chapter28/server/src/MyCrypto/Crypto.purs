module MyCrypto.Crypto where

import Data.Char (toCharCode)
import Data.Foldable (foldl)
import Data.String.CodeUnits (fromCharArray, toCharArray, length)
import Effect (Effect)
import Node.Buffer (Buffer, fromString, toString)
import Node.Encoding (Encoding(UTF8, Hex))
import Prelude
import Random.LCG (Seed, mkSeed)
import Test.QuickCheck.Arbitrary (arbitrary)
import Test.QuickCheck.Gen (sample)

data Algorithm
  = MD5
  | SHA256
  | SHA512
  | SHA1

instance Show Algorithm where
  show MD5    = "md5"
  show SHA256 = "sha256"
  show SHA512 = "sha512"
  show SHA1   = "sha1"

foreign import digest :: Hash -> Effect Buffer
foreign import update :: Hash -> Buffer -> Effect Hash
foreign import _createHash :: String -> Effect Hash
foreign import data Hash :: Type

createHash :: Algorithm -> Effect Hash
createHash alg = _createHash $ show alg

hash :: Algorithm -> String -> Encoding -> Effect String
hash alg str enc = do
  buf <- fromString str UTF8
  createHash alg >>= flip update buf >>= digest >>= toString enc

hex :: Algorithm -> String -> Effect String
hex alg str = hash alg str Hex

userNameSeed :: String -> Seed
userNameSeed = mkSeed <<< foldl (*) 1 <<< map toCharCode <<< toCharArray

userNameSalt :: Int -> String -> String
userNameSalt saltLength userName = fromCharArray $ sample (userNameSeed userName) saltLength arbitrary

passwordHashHex :: String -> String -> Effect String
passwordHashHex userName password =
  let salt = userNameSalt (3 * length userName) userName in
  hex SHA512 (password <> salt)
