module Twilio.Config
  ( TwilioConfig
  , loadTwilioConfig
  ) where

import Data.Either (Either)
import Type.Data.Row (RProxy(..))
import Twilio.Request (AuthToken)
import Utils.Env (Env, fromEnv, type (<:), EnvError)

type TwilioConfigProxy
  = ( accountId :: String <: "TWILIO_ACCOUNT_ID"
    -- , apiRoot :: String <: "TWILIO_API"
    , authToken :: AuthToken <: "TWILIO_AUTH_TOKEN"
    )

type TwilioConfig
  = { accountId :: String
    , authToken :: AuthToken
    -- , apiRoot :: String
    }

loadTwilioConfig :: Env -> Either EnvError TwilioConfig
loadTwilioConfig env = fromEnv (RProxy :: RProxy TwilioConfigProxy) env
