module Handler.Class.ApiHandler where

import Control.Monad.Reader (ReaderT)
import Effect.Aff (Aff)
import Effect.Aff.AVar (AVar)
import HTTPure.Response (Response)
import Manager.Account (Accounts)
import Manager.Session (Sessions)

type HandlerEnv =
  { accountsAVar :: AVar Accounts
  , sessionsAVar :: AVar Sessions
  }
-- NOTE: `ResponseM` is type alias for `Aff Response` 
type Handler = ReaderT HandlerEnv Aff Response

-- NOTE: 'Constraint' is the 'Type' for a Typeclass
--class ApiHandler :: forall k. k -> Constraint
-- NOTE: Definition of Proxy is `data Proxy a = Proxy` where the 'a' Type is a so called Phantom Type
-- without the Proxy we get following type error:
-- 1.   The declaration handle contains arguments that couldn't be determined.
--      These arguments are: { a }
--class ApiHandler a where
--  handle :: Proxy a -> String -> Either JsonDecodeError Handler

class RequestHandler req where
  handle :: req -> Handler
