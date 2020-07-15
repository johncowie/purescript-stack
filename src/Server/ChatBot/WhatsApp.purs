module Server.ChatBot.WhatsApp
( WhatsAppBot
, whatsAppBot )
where

import Prelude
import Data.Either (Either(..))
import Data.Tuple.Nested ((/\))
import Twilio.WhatsApp (WhatsAppMessage, WhatsAppNumber)
import Twilio.WhatsApp as WA
import Server.ChatBot (ChatBot, ChatBotStore)
import Utils.ExceptT (runExceptT, ExceptT(..))

{-
  Type for handling whatsapp messages

  Is going to need a:
    - ChatBotStore (i.e. means of storing chat state)
    - ChatBot (i.e. means of handling the message)


  TODO [ ] defined WhatsAppBot type
       [ ] create possibilty for no response (need function for empty message response)
       [ ] constructor for WhatsAppBot type
       [ ] in-memory implementation of Dunbar ChatBotStore
       [ ] in-memory implementation of Dunbar ChatBot
       [ ] wire it all together
-}

type WhatsAppBot m = {
  handleMessage :: WhatsAppMessage -> m (Either String WhatsAppMessage)
}

handleMessage :: forall m st.
                 (Monad m)
              => ChatBot m st
              -> ChatBotStore m WhatsAppNumber st
              -> WhatsAppMessage
              -> m (Either String WhatsAppMessage)
handleMessage bot botStore msg = runExceptT $ WA.replyToMessageM msg \id msgBody -> do
  state <- ExceptT $ botStore.retrieveState id
  (newState /\ response) <- ExceptT $ map Right $ bot.handleMessage state msgBody
  ExceptT $ botStore.storeState id newState
  pure response


whatsAppBot :: forall m st. (Monad m) => ChatBot m st -> ChatBotStore m WhatsAppNumber st -> WhatsAppBot m
whatsAppBot bot store = {
  handleMessage: handleMessage bot store
}
