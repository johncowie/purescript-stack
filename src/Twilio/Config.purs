module Twilio.Config
  ( TwilioConfig
  , twilioEnvVars
  ) where

import Prelude
import Twilio.Request (AuthToken)
import Envisage (Var, describe, showParsed)
import Envisage.Var (var)

twilioEnvVars :: { accountId :: Var String
                 , authToken :: Var AuthToken}
twilioEnvVars = { accountId: var "TWILIO_ACCOUNT_ID" # showParsed # describe "Twillio account Id"
                , authToken: var "TWILIO_AUTH_TOKEN" # describe "Twilio auth token"}

type TwilioConfig
  = { accountId :: String
    , authToken :: AuthToken
    -- , apiRoot :: String
    }
