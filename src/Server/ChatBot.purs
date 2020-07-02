module Server.ChatBot where

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

-}

-- type ChatBotApi = 
