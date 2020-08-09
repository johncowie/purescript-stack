module Twilio.WhatsApp
  ( WhatsAppMessage
  , WhatsAppNumber
  , WhatsAppDeliveryNotification
  , WhatsAppDeliveryStatus
  , SID
  , showWhatsAppNumber
  , toTwiml
  , toTwimlMaybe
  , replyToMessage
  , replyToMessageM
  ) where

import Prelude
import Data.Identity (Identity(..))
import Data.Maybe (Maybe(..), maybe)
import Data.Either (Either(..))
import Data.Argonaut.Decode (class DecodeJson, decodeJson)
import Data.Newtype (unwrap)
import Data.String as Str
import Effect.Aff (Aff)
import Twilio.Twiml as Twiml
import Twilio.Config (TwilioConfig)
import JohnCowie.HTTPure.Middleware.FormURLEncoded (class DecodeFormData, formDataValue)

{-
Authentication: need accountId and authToken

Config: accountId, authToken, apiRoot

Sending message: send form data with to, from, body, plus auth in header
 - returns SID

handle delivery notification - has SID and TO, plus auth stuff

handle received message - has to, from, body, plus auth stuff

-}
newtype SID
  = SID String

newtype WhatsAppNumber
  = WhatsAppNumber String

instance decodeJsonWhatsAppNumber :: DecodeJson WhatsAppNumber where
  decodeJson json = decodeJson json >>= whatsAppNumber

derive instance ordWhatsAppNumber :: Ord WhatsAppNumber

derive instance eqWhatsAppNumber :: Eq WhatsAppNumber

newtype WhatsAppMessage
  = WhatsAppMessage
  { from :: WhatsAppNumber
  , to :: WhatsAppNumber
  , message :: String
  }

showWhatsAppNumber :: WhatsAppNumber -> String
showWhatsAppNumber (WhatsAppNumber s) = s

messageSender :: WhatsAppMessage -> WhatsAppNumber
messageSender (WhatsAppMessage { from }) = from

messageBody :: WhatsAppMessage -> String
messageBody (WhatsAppMessage { message }) = message

instance decodeFormDataWhatsAppMessage :: DecodeFormData WhatsAppMessage where
  decodeFormData formData = do
    message <- formDataValue "Body" formData
    to <- formDataValue "To" formData >>= whatsAppNumber
    from <- formDataValue "From" formData >>= whatsAppNumber
    pure $ WhatsAppMessage { message, from, to }

instance decodeJsonWhatsAppMessage :: DecodeJson WhatsAppMessage where
  decodeJson json = WhatsAppMessage <$> decodeJson json

data WhatsAppDeliveryStatus
  = Undelivered
  | Failed
  | Delivered
  | Sent
  | Read
  | Unsupported String

instance showWhatsAppDeliveryStatus :: Show WhatsAppDeliveryStatus where
  show Undelivered = "Undelivered"
  show Failed = "Failed"
  show Delivered = "Delivered"
  show Sent = "Sent"
  show Read = "Read"
  show (Unsupported s) = "Unsupported Status: [" <> s <> "]"

newtype WhatsAppDeliveryNotification
  = WhatsAppDeliveryNotification
  { status :: WhatsAppDeliveryStatus
  , sid :: SID
  , recipient :: WhatsAppNumber
  }

instance showWhatsAppDeliveryNotification :: Show WhatsAppDeliveryNotification where
  show ( WhatsAppDeliveryNotification
      { status
    , sid: (SID sid)
    , recipient: (WhatsAppNumber recipient)
    }
  ) =
    show
      { status: show status
      , sid
      , recipient
      }

instance decodeFormDataWhatsAppDeliveryNotification :: DecodeFormData WhatsAppDeliveryNotification where
  decodeFormData formData = do
    sid <- SID <$> formDataValue "MessageSid" formData
    recipient <- formDataValue "To" formData >>= whatsAppNumber
    status <- deliveryStatus <$> formDataValue "MessageStatus" formData
    pure $ WhatsAppDeliveryNotification { sid, recipient, status }

deliveryStatus :: String -> WhatsAppDeliveryStatus
deliveryStatus s = case (Str.toLower s) of
  "undelivered" -> Undelivered
  "delivered" -> Delivered
  "failed" -> Failed
  "sent" -> Sent
  "read" -> Read
  other -> Unsupported other

whatsAppNumber :: String -> Either String WhatsAppNumber
whatsAppNumber = WhatsAppNumber >>> Right

-- TODO strip out whatsapp: prefix
-- TODO return error if phonenumber format is invalid
sendMessage :: TwilioConfig -> WhatsAppMessage -> Aff (Either String SID)
sendMessage config msg = pure (Right (SID "")) -- TODO implement me

toNumber :: WhatsAppNumber -> Twiml.To
toNumber (WhatsAppNumber number) = Twiml.to number

fromNumber :: WhatsAppNumber -> Twiml.From
fromNumber (WhatsAppNumber number) = Twiml.from number

toTwiml :: WhatsAppMessage -> Twiml.TwimlString
toTwiml (WhatsAppMessage { to, from, message }) = Twiml.messagingResponse (toNumber to) (fromNumber from) (Twiml.message message)

toTwimlMaybe :: Maybe WhatsAppMessage -> Twiml.TwimlString
toTwimlMaybe = maybe Twiml.emptyMessagingResponse toTwiml

replyToMessageM ::
  forall m.
  (Bind m) =>
  (Applicative m) =>
  WhatsAppMessage ->
  (WhatsAppNumber -> String -> m (Maybe String)) -> m (Maybe WhatsAppMessage)
replyToMessageM (WhatsAppMessage { to, from, message }) responseF = do
  responseM <- responseF from message
  case responseM of
    (Just response) ->
      pure $ Just
        $ WhatsAppMessage
            { to: from
            , from: to
            , message: response
            }
    Nothing -> pure $ Nothing

replyToMessage :: WhatsAppMessage -> (WhatsAppNumber -> String -> Maybe String) -> Maybe WhatsAppMessage
replyToMessage msg f = unwrap $ replyToMessageM msg (\a b -> Identity $ f a b)
