module Dunbar.ChatBot where

import Prelude
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Argonaut.Decode (class DecodeJson, decodeJson)
import Data.Maybe(Maybe(..), fromMaybe)
import Data.Either (Either(..))
import Data.Tuple.Nested(type (/\), (/\))
import Data.Map as M

import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Ref as Ref

import Server.ChatBot (ChatBot, ChatBotStore)
import Server.ChatBot.WhatsApp (WhatsAppBot, whatsAppBot)

import Undefined (undefined)

data ChatState = New | Idle | AwaitingName

instance encodeJsonChatState :: EncodeJson ChatState where
  encodeJson Idle = encodeJson "Idle"
  encodeJson AwaitingName = encodeJson "AwaitingName"
  encodeJson New = encodeJson "New"

instance decodeJsonChatState :: DecodeJson ChatState where
  decodeJson json = do
    str <- decodeJson json
    case str of
      "Idle" -> pure Idle
      "AwaitingName" -> pure AwaitingName
      "New" -> pure New
      s -> Left $ "Failed to parse state: " <> s

data UserMessage = WakeUp | AddFriend | SpecifyName String

data BotMessage = Intro | Misunderstood | RequestName

parseUserMessage :: String -> ChatState -> Maybe UserMessage
parseUserMessage _ New = Just WakeUp
parseUserMessage "add" Idle = Just AddFriend
parseUserMessage name AwaitingName = Just (SpecifyName name)
parseUserMessage _ _ = Nothing

handleUserMessage :: UserMessage -> ChatState -> Aff (ChatState /\ BotMessage)
handleUserMessage WakeUp New = pure $ Idle /\ Intro
handleUserMessage _ st = pure $ st /\ Misunderstood

showBotMessage :: BotMessage -> String
showBotMessage Intro = """Hi, welcome to John's app for keeping track of your friends.
                          Reply with 'add' to add a friend."""
showBotMessage RequestName = """What's the name of your friend?"""
showBotMessage Misunderstood = """Sorry, I didn't understand what you said."""

processMessage :: ChatState -> String -> Aff (ChatState /\ String)
processMessage state msg = fromMaybe (pure (state /\ showBotMessage Misunderstood)) $ do
  userMsg <- parseUserMessage msg state
  pure $ map (map showBotMessage) $ handleUserMessage userMsg state

inMemoryChatBotStore :: forall id st. (Ord id) => st -> Aff (ChatBotStore Aff id st)
inMemoryChatBotStore newChatState = do
  ref <- liftEffect $ Ref.new M.empty
  pure { storeState: \id st -> liftEffect $ map Right $ Ref.modify_ (M.insert id st) ref
       , retrieveState: \id -> liftEffect $ map Right $ do
           m <- Ref.read ref
           pure $ fromMaybe newChatState $ M.lookup id m
       }

chatBot :: ChatBot Aff ChatState
chatBot = {
  handleMessage: processMessage
}

dunbarWhatsAppBot :: Aff (WhatsAppBot Aff)
dunbarWhatsAppBot = do
  store <- inMemoryChatBotStore New
  pure $ whatsAppBot chatBot store
