module Twilio.Twiml
( TwimlString
, To
, From
, Message
, to
, from
, message
, messagingResponse
, toString )
where

import Prelude (($))

newtype TwimlString = TwimlString String

toString :: TwimlString -> String
toString (TwimlString s) = s

newtype To = To String

to :: String -> To
to = To

newtype From = From String

from :: String -> From
from = From

newtype Message = Message String

message :: String -> Message
message = Message

messagingResponse :: To -> From -> Message -> TwimlString
messagingResponse (To to_) (From from_) (Message msg_) = TwimlString $ _messagingResponse to_ from_ msg_

foreign import _messagingResponse :: String -> String -> String -> String
