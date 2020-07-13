module Twilio.WhatsApp
( WhatsAppMessage
, WhatsAppNumber
, toTwiml
, replyToMessage
)
where

import Prelude

import Affjax.RequestBody (RequestBody(..))

import Data.Either (Either(..))
import Data.Argonaut.Core (Json)
import Data.Argonaut.Parser (jsonParser)
import Data.Argonaut.Decode (class DecodeJson, (.:), decodeJson)
import Data.Newtype (wrap)

import Effect.Aff (Aff)
import Type.Data.Row (RProxy(..))
import Undefined (undefined)
import Utils.Env (Env, type (<:), EnvError, fromEnv)

import Twilio.Twiml as Twiml

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

instance decodeJsonWhatsAppNumber :: DecodeJson WhatsAppNumber where
  decodeJson json = decodeJson json >>= whatsAppNumber

newtype WhatsAppMessage = WhatsAppMessage {
  from :: WhatsAppNumber
, to :: WhatsAppNumber
, message :: String
}

instance decodeJsonWhatsAppMessage :: DecodeJson WhatsAppMessage where
  decodeJson json = do
    obj <- decodeJson json
    message <- obj .: "Body"
    to <- obj .: "To"
    from <- obj .: "From"
    pure $ WhatsAppMessage {message, from, to}

type WhatsAppConfigProxy = (
  accountId :: String <: "TWILIO_ACCOUNT_ID"
, apiRoot :: String <: "TWILIO_API"
, authToken :: String <: "TWILIO_AUTH_TOKEN"
)

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

type WhatsAppParser = {
  validateMessageSignature :: String -> Either String Unit
}

type MsgPayload = {
  body :: String
, from :: WhatsAppNumber
, to :: WhatsAppNumber
}

whatsAppNumber :: String -> Either String WhatsAppNumber
whatsAppNumber = WhatsAppNumber >>> Right
-- TODO strip out whatsapp: prefix
-- TODO return error if phonenumber format is invalid

loadConfig :: Env -> Either EnvError WhatsAppConfig
loadConfig env = WhatsAppConfig <$> fromEnv (RProxy :: RProxy WhatsAppConfigProxy) env

sendMessage :: WhatsAppConfig -> WhatsAppMessage -> Aff (Either String SID)
sendMessage config msg = pure (Right (SID "")) -- TODO implement me

toNumber :: WhatsAppNumber -> Twiml.To
toNumber (WhatsAppNumber number) = Twiml.to number

fromNumber :: WhatsAppNumber -> Twiml.From
fromNumber (WhatsAppNumber number) = Twiml.from number

toTwiml :: WhatsAppMessage -> Twiml.TwimlString
toTwiml (WhatsAppMessage {to, from, message}) =
  Twiml.messagingResponse (toNumber to) (fromNumber from) (Twiml.message message)

replyToMessage :: WhatsAppMessage -> (WhatsAppNumber -> String -> String) -> WhatsAppMessage
replyToMessage (WhatsAppMessage {to, from, message}) responseF =
  WhatsAppMessage { to: from
                  , from: to
                  , message: responseF from message}
