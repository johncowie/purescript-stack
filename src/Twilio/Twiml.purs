module Twilio.Twiml
  ( TwimlString
  , To
  , From
  , Message
  , to
  , from
  , message
  , messagingResponse
  , emptyMessagingResponse
  , toString
  ) where

import Prelude (($))

newtype TwimlString
  = TwimlString String

toString :: TwimlString -> String
toString (TwimlString s) = s

newtype To
  = To String

to :: String -> To
to = To

newtype From
  = From String

from :: String -> From
from = From

newtype Message
  = Message String

message :: String -> Message
message = Message

messagingResponse :: To -> From -> Message -> TwimlString
messagingResponse (To to_) (From from_) (Message msg_) = TwimlString $ _messagingResponse to_ from_ msg_

emptyMessagingResponse :: TwimlString
emptyMessagingResponse = TwimlString _emptyMessagingResponse

-- TODO create type for AuthToken
-- TODO create type for Signature
foreign import _messagingResponse :: String -> String -> String -> String

foreign import _emptyMessagingResponse :: String
