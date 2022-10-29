module Parser.Account where

import Data.Maybe (Maybe(..))
import Control.Alt ((<|>))
import Data.Array (many, (:), some, fromFoldable, catMaybes)
import Data.CodePoint.Unicode (isAlpha, isAlphaNum, isUpper, isLower)
import Data.Identity (Identity)
import Data.String.CodePoints (codePointFromChar)
import Data.String.CodeUnits (fromCharArray)
import Entity.Account (Account(..))
import Parsing (ParserT, fail)
import Parsing.Combinators (sepBy)
import Parsing.String (satisfy, char, string)
import Prelude

type AccountParserT a = ParserT String Identity a

userName :: AccountParserT String
userName = do
  alpha     :: Char       <- satisfy (isAlpha <<< codePointFromChar)
  alphaNums :: Array Char <- many $ satisfy (isAlphaNum <<< codePointFromChar)
  pure $ fromCharArray $ alpha : alphaNums

hex :: AccountParserT String
hex = fromCharArray <$> (some $ satisfy isHex)
  where isHex c = (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f')

passwordHash :: AccountParserT String
passwordHash = hex

boolean :: String -> AccountParserT Boolean
boolean fieldName = (string "true" *> pure true)
  <|> (string "false" *> pure true)
  <|> fail ("invalid bool for " <> fieldName)

temporaryPassword :: AccountParserT Boolean
temporaryPassword = boolean "temporaryPassword"

admin :: AccountParserT Boolean
admin = boolean "admin"

properName :: AccountParserT String
properName = do
  first :: Char       <- satisfy (isUpper <<< codePointFromChar)
  rest  :: Array Char <- many $ satisfy (isLower <<< codePointFromChar)
  pure $ fromCharArray (first : rest)

firstName :: AccountParserT String
firstName = properName

lastName :: AccountParserT String
lastName = properName

comma :: forall a. AccountParserT a -> AccountParserT a
--comma p = char ',' *> p
comma p = p <* char ','

accountParser :: AccountParserT Account
accountParser = do
  userNameVal          <- userName # comma
  passwordHashVal      <- passwordHash # comma
  temporaryPasswordVal <- temporaryPassword # comma
  adminVal             <- admin # comma
  firstNameVal         <- firstName # comma
  lastNameVal          <- lastName
  pure $ Account
    { userName          : userNameVal
    , passwordHash      : passwordHashVal
    , temporaryPassword : temporaryPasswordVal
    , admin             : adminVal
    , firstName         : firstNameVal
    , lastName          : lastNameVal
    }

accountsParser :: AccountParserT (Array Account)
accountsParser =
  (sepBy (Just <$> accountParser <|> pure Nothing) $ char '\n')
  <#> catMaybes <<< fromFoldable

--testVal :: String
--testVal = "admin,f4cf17b2a9bae2e92974644a200eeee2b5a9096b1a502643cafe26b9e1fd21469d784e462e66290efe00ca9a1ca4da2767d0cce96372ab284100a221c79d3cce,true,true,Joe,Admin"
--
--test :: Identity (Either String String)
--test = do
--  res <- runParserT testVal accountParser
--  let foo = bimap show show res
--  pure foo
--
--testHash :: String
--testHash = "f4cf17b2a9bae2e92974644a200eeee2b5a9096b1a502643cafe26b9e1fd21469d784e462e66290efe00ca9a1ca4da2767d0cce96372ab284100a221c79d3cce"
--
--test2 :: Identity (Either ParseError String)
--test2 = runParserT testHash (passwordHash >>= (admin # comma)) 
