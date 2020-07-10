module Twilio.WhatsApp where

import Data.Either (Either(..))

import Effect.Aff (Aff)

import Undefined (undefined)
import Utils.Env (Env)

{-
Authentication: need accountId and authToken

Config: accountId, authToken, apiRoot

Sending message: send form data with to, from, body, plus auth in header
 - returns SID

handle delivery notification - has SID and TO, plus auth stuff

handle received message - has to, from, body, plus auth stuff

-}

newtype SID = SID String

newtype WhatsAppNumber = WhatsAppNumber String

newtype WhatsAppMessage = WhatsAppMessage {
  from :: WhatsAppNumber
, to :: WhatsAppNumber
, message :: String
}

newtype WhatsAppConfig = WhatsAppConfig {
  accountId :: String
, authToken :: String
, apiRoot :: String
}

type WhatsAppClient = {
  sendMessage :: WhatsAppMessage -> Aff (Either String SID)
  -- TODO receive message
  -- TODO receive delivery notification
}

whatsAppNumber :: String -> Either String WhatsAppNumber
whatsAppNumber = undefined

loadConfig :: Env -> WhatsAppConfig
loadConfig = undefined

sendMessage :: WhatsAppConfig -> WhatsAppMessage -> Aff (Either String SID)
sendMessage = undefined
