module Manager.Account where

import Data.Array (fromFoldable)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Map (Map)
import Data.Map as Map
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Effect.Aff.AVar (AVar)
import Effect.Aff.AVar as AVar
import Effect.Class (liftEffect)
import Entity.Account (Account(..))
import MyCrypto.Crypto (passwordHashHex)
import Prelude
import Utils (withAVar)

type Accounts = Map String Account

startup :: Array Account -> Aff (AVar Accounts)
startup accounts =
  accounts
    <#> (\account@(Account {userName}) -> Tuple userName account)
    # Map.fromFoldable
    # AVar.new

shutdown :: AVar Accounts -> Aff Unit
shutdown = void <<< AVar.take

verifyLogon :: AVar Accounts -> String -> String -> Aff (Maybe Account)
verifyLogon accountsAVar userName password = do
  pwHash :: String <- liftEffect $ passwordHashHex userName password
  accounts <- AVar.read accountsAVar
  accounts
    # Map.lookup userName
    >>= (\account@(Account {passwordHash}) ->
      if pwHash == passwordHash
      then Just account
      else Nothing)
    # pure

data CreateAccountError = CreateAccountAlreadyExists

createAccount :: AVar Accounts -> Account -> Aff (Either CreateAccountError Unit)
createAccount accountsAVar account@(Account { userName }) = do
  withAVar accountsAVar
    \accounts -> pure $
      if Map.member userName accounts then
        Tuple accounts (Left CreateAccountAlreadyExists)
      else
        Tuple (Map.insert userName account accounts) (Right unit)

getAccounts :: AVar Accounts -> Aff (Array Account)
getAccounts accountsAVar = do
  accounts <- AVar.read accountsAVar
  pure $ fromFoldable $ Map.values accounts

findAccount :: AVar Accounts -> String -> Aff (Maybe Account)
findAccount accountsAVar userName = do
  accounts <- AVar.read accountsAVar
  pure $ Map.lookup userName accounts
