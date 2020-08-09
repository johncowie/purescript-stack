module Twilio.Config
  ( TwilioConfig
  , twilioEnvVars
  ) where

import Prelude
import Data.Newtype (wrap, unwrap)
import Data.Either (Either(..))
import Twilio.Request (AuthToken)
import Envisage (Var, describe, withShow)
import Envisage.Var (var, newVar)

twilioEnvVars :: { accountId :: Var String
                 , authToken :: Var AuthToken}
twilioEnvVars = { accountId: var "TWILIO_ACCOUNT_ID"
                , authToken: var "TWILIO_AUTH_TOKEN"}

type TwilioConfig
  = { accountId :: String
    , authToken :: AuthToken
    -- , apiRoot :: String
    }
