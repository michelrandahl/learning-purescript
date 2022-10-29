module Handler.Account where

import Control.Monad.Error.Class (try)
import Data.Bifunctor (lmap)
import Data.Either (Either)
import Data.Foldable (intercalate)
import Data.Newtype (unwrap)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Entity.Account (Account(..))
import MyCrypto.Crypto (passwordHashHex)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile, writeTextFile, appendTextFile)
import Node.FS.Sync (exists)
import Parser.Account (accountsParser)
import Parsing (runParserT, ParseError)
import Prelude


bootstrapAccount :: Effect String
bootstrapAccount = do
  let userName = "admin"
      password = "admin"
  passwordHash <- passwordHashHex userName password
  --let true' = show true
  --pure $ intercalate "," [userName, passwordHash, true', true', "Joe", "Admin"]
  pure $ accountToCSV $ Account
    { userName
    , passwordHash
    , temporaryPassword: true
    , admin: true
    , firstName: "Joe"
    , lastName: "Admin"
    }

accountsFile :: String
accountsFile = "accounts.csv"

loadAccounts :: Aff (Either ParseError (Array Account))
loadAccounts = do
  exist :: Boolean <- liftEffect $ exists accountsFile
  bsa :: String <- liftEffect bootstrapAccount
  when (not exist) $ writeTextFile ASCII accountsFile bsa
  --accountLines :: Array String <- lines <$> readTextFile ASCII accountsFile
  --pure $ sequence $ (unwrap <<< flip runParserT accountParser) <$> accountLines
  fileData <- readTextFile ASCII accountsFile
  pure $ unwrap $ runParserT fileData accountsParser

accountToCSV :: Account -> String
accountToCSV (Account {userName,passwordHash,temporaryPassword,admin,firstName,lastName}) =
  intercalate ","
    [ userName
    , passwordHash
    , show temporaryPassword
    , show admin
    , firstName
    , lastName
    ] <> "\n"

data CreateAccountError = CreateAccountFileError String

-- TODO use the parser on the CSV line to verify it
-- for example user could have entered firstName and lastName starting with lowercase
-- ... can we throw more descriptive errors from the parser to communicate the error or should we perhaps also parse and report error for the json input?
createAccount :: Account -> Aff (Either CreateAccountError Unit)
createAccount account =
  lmap show <$> (try $ appendTextFile ASCII accountsFile $ accountToCSV account)
    <#> lmap CreateAccountFileError
