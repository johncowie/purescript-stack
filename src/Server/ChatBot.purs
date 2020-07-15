module Server.ChatBot where

import Prelude
import Data.DateTime.Instant (Instant)
import Data.Either (Either(..))
import Data.Tuple (Tuple)
import Effect.Aff (Aff)

{-
 - Want to create a generalised abstraction for an asynchronous chatbot
 *Types of interaction for a Chatbot API*

 - Receive a message
 - Respond directly to a received message
 - Send a message
 - Receive delivery status for a message

 - handling template failures and responding with a new message is potentially an abstraction around this one

 *Design of actual chatbot, need some sort of state machine type thing*

 - broadly speaking, interactions are defined by pairs of message-checker state-checker and response generator
 - i.e. for a given message and a given state, a response should be produced
 - types are Message, State, Response

 - need type for storing and retrieving user states
-}


--- how to use eventStore not through API?
--  could have an interface for eventStore with a HTTP client implementation and a direct DB implementation

type ChatBotStore m id st = {
  storeState :: id -> st -> m (Either String Unit)
, retrieveState :: id -> m (Either String st)
}

type ChatBot m st = {
  handleMessage :: st -> String -> m (Tuple st String) -- TODO should support array of messages
-- , handleDeliveryStatus :: state -> deliveryStatus -> Tuple state (Array msg)
  -- some function for tick - i.e. generating messages that are not sent to music
}

-- create WhatsApp implementation that uses this to progress states
