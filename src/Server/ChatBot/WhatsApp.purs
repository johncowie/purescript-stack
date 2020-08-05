module Server.ChatBot.WhatsApp
  ( WhatsAppBot
  , class ToWhatsAppMsg
  , toWhatsAppMsg
  , whatsAppBot
  ) where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Either (Either)
import Data.Tuple.Nested ((/\))
import Data.String as Str
import Twilio.WhatsApp (WhatsAppMessage, WhatsAppNumber)
import Twilio.WhatsApp as WA
import Server.ChatBot (ChatBot, ChatBotStore)
import Utils.ExceptT (runExceptT, ExceptT(..))

class ToWhatsAppMsg o where
  toWhatsAppMsg :: o -> String

instance toWhatsAppMsgString :: ToWhatsAppMsg String where
  toWhatsAppMsg = identity

type WhatsAppBot m
  = { handleMessage :: WhatsAppMessage -> m (Either String (Maybe WhatsAppMessage))
    }

emptyToNothing :: String -> Maybe String
emptyToNothing "" = Nothing

emptyToNothing s = Just s

handleMessage ::
  forall m st o.
  (ToWhatsAppMsg o) =>
  (Monad m) =>
  ChatBot m WhatsAppNumber st o ->
  ChatBotStore m WhatsAppNumber st ->
  WhatsAppMessage ->
  m (Either String (Maybe WhatsAppMessage))
handleMessage bot botStore msg =
  runExceptT
    $ WA.replyToMessageM msg \id msgBody -> do
        state <- ExceptT $ botStore.retrieveState id
        (newState /\ responses) <- ExceptT $ bot.handleMessage id state msgBody
        ExceptT $ botStore.storeState id newState
        pure $ emptyToNothing $ Str.joinWith "\n\n" $ map toWhatsAppMsg $ responses -- TODO send rest of the messages asynchronously

whatsAppBot :: forall m st o. (ToWhatsAppMsg o) => (Monad m) => ChatBot m WhatsAppNumber st o -> ChatBotStore m WhatsAppNumber st -> WhatsAppBot m
whatsAppBot bot store =
  { handleMessage: handleMessage bot store
  }
