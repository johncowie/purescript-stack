module Dunbar.ChatBot where

import Prelude

import Data.Argonaut.Decode (class DecodeJson, decodeJson, (.:))
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Array (sort)
import Data.Either (Either(..))
import Data.Foldable (foldr, oneOf, maximum)
import Data.Int as Int
import Data.Map as M
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (wrap)
import Data.String as Str
import Data.String.Regex (replace, split) as Re
import Data.String.Regex.Flags (global) as Re
import Data.String.Regex.Unsafe (unsafeRegex) as Re
import Data.Traversable (for)
import Data.Tuple (fst, snd)
import Data.Tuple.Nested (type (/\), (/\))
import Dunbar.Friend as Friend
import Dunbar.State as St
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Server.ChatBot (ChatBot, ChatBotStore)
import Server.ChatBot.WhatsApp (WhatsAppBot, whatsAppBot, class ToWhatsAppMsg)
import Server.DB (DB)
import Server.DB as DB
import Server.Domain (OAuthProvider(WhatsApp), UserId)
import Twilio.WhatsApp (WhatsAppNumber, showWhatsAppNumber)
import Utils.ExceptT (ExceptT(..), runExceptT, showError)
import Utils.Lens as L

data ChatState = New
               | Idle
               | AwaitingFirstName
               | AwaitingLastName {firstName :: String}
               | AwaitingContactFreq {firstName :: String, lastName :: String}

data BotMessage = Intro
                | Misunderstood
                | RequestFirstName
                | RequestLastName String
                | RequestContactFreq String String
                | AcknowledgeName String String
                | FriendsList (Array String)

type Deps = {
  db :: DB
}

chatStateRecord :: forall r. String -> r -> {state :: String, stateData :: r}
chatStateRecord state stateData = {state, stateData}

instance encodeJsonChatState :: EncodeJson ChatState where
  encodeJson Idle = encodeJson $ chatStateRecord "Idle" {}
  encodeJson AwaitingFirstName = encodeJson $ chatStateRecord "AwaitingFirstName" {}
  encodeJson (AwaitingLastName r) = encodeJson $ chatStateRecord "AwaitingLastName" r
  encodeJson (AwaitingContactFreq r) = encodeJson $ chatStateRecord "AwaitingContactFreq" r
  encodeJson New = encodeJson $ chatStateRecord "New" {}

instance decodeJsonChatState :: DecodeJson ChatState where
  decodeJson json = do
    obj <- decodeJson json
    stateType <- obj .: "state"
    stateData <- obj .: "stateData"
    case stateType of
      "Idle" -> pure Idle
      "AwaitingFirstName" -> pure AwaitingFirstName
      "AwaitingLastName" ->  AwaitingLastName <$> decodeJson stateData
      "AwaitingContactFreq" -> AwaitingContactFreq <$> decodeJson stateData
      "New" -> pure New
      s -> Left $ "Failed to parse state: " <> s

instance toWhatsAppMsgBotMessage :: ToWhatsAppMsg BotMessage where
  toWhatsAppMsg = showBotMessage

showBotMessage :: BotMessage -> String
showBotMessage Intro = """Hi, welcome to John's app for keeping track of your friends. Reply with 'add' to add a friend."""
showBotMessage RequestFirstName = """What's the first name of your friend?"""
showBotMessage (RequestLastName fn) = "What's *" <> fn <> "*'s last name?"
showBotMessage (RequestContactFreq fn ln) = "How often do you want to be \
\in touch with *" <> fn <> " " <> ln <> """*? \\nRespond with an interval (e.g. _every 2 weeks_, _every 3 months_)"""
showBotMessage Misunderstood = """Sorry, I didn't understand what you said."""
showBotMessage (AcknowledgeName fn ln) = "Thanks, you added *" <> fn <> " " <> ln <> "*."
showBotMessage (FriendsList friendNames) =
  "Your friends: \n\n" <> Str.joinWith "\n" friendNames

storeEvents :: DB -> WhatsAppNumber -> Array St.Event ->  Aff (Either String Unit)
storeEvents db waNumber events = runExceptT do
  uid <- ExceptT $ getUserId db waNumber
  void $ for events $ \event -> do
    ExceptT $ map showError $ DB.addEvent uid (wrap "Dunbar") event db
  -- TODO store snapshot

loadState :: DB -> WhatsAppNumber -> Aff (Either String St.State)
loadState db waNumber = runExceptT do
  uid <- ExceptT $ getUserId db waNumber
  snapshotTupleM <- ExceptT $ map showError $ DB.retrieveLatestSnapshot uid (wrap "Dunbar") db
  case snapshotTupleM  of
      Just (snapshot /\ upToEvent) -> do
        eventsAndIds <- ExceptT $ map showError $ DB.retrieveEvents uid (wrap "Dunbar") (Just upToEvent) db
        pure $ foldr St.processEvent snapshot (map snd eventsAndIds)
      Nothing -> do
        eventsAndIds <- ExceptT $ map showError $ DB.retrieveEvents uid (wrap "Dunbar") Nothing db
        pure $ foldr St.processEvent St.empty (map snd eventsAndIds)

stripPunctuation :: String -> String
stripPunctuation = Re.replace regex ""
  where regex = Re.unsafeRegex "[^a-zA-Z0-9\\s]" Re.global

anyMsg :: String -> Maybe String
anyMsg = Just

ignoreMsg :: String -> Maybe Unit
ignoreMsg _ = Just unit

isCommand :: String -> String -> Maybe Unit
isCommand cmd input = if (cleaned == cmd) then Just unit else Nothing
  where cleaned = Str.toLower $ Str.trim $ stripPunctuation input

words :: String -> Array String
words = Re.split regex
  where regex = Re.unsafeRegex "\\s+" Re.global

tokens :: String -> Array String
tokens = words >>> map (stripPunctuation >>> Str.toLower)

parseAmount :: String -> Maybe Int
parseAmount s = do
  i <- Int.fromString s
  if (i > 0) then Just i else Nothing

parseTimeUnit :: String -> Maybe Int
parseTimeUnit "day" = Just 1
parseTimeUnit "days" = Just 1
parseTimeUnit "week" = Just 7
parseTimeUnit "weeks" = Just 7
parseTimeUnit "month" = Just 30
parseTimeUnit "months" = Just 30
parseTimeUnit "year" = Just 365
parseTimeUnit "years" = Just 365
parseTimeUnit _ = Nothing

parseDuration :: Array String -> Maybe Int
parseDuration [amount, unit] = do
  a <- parseAmount amount
  u <- parseTimeUnit unit
  pure $ a * u
parseDuration ["every", amount, unit] = parseDuration [amount, unit]
parseDuration _ = Nothing

type BotResult = Aff (Either String (ChatState /\ Array BotMessage))

tryTo :: forall a. (String -> Maybe a) -> (a -> BotResult) -> (String -> Maybe BotResult)
tryTo parser executor s = do
  v <- parser s
  pure $ executor v

pureResult :: ChatState -> Array BotMessage -> BotResult
pureResult st msgs = pure $ Right $ st /\ msgs

misunderstood :: ChatState -> String -> BotResult
misunderstood st _ = pureResult st [Misunderstood]

welcomeUser :: Unit -> BotResult
welcomeUser _ = pureResult Idle [Intro]

requestFirstName :: Unit -> BotResult
requestFirstName _ = pureResult AwaitingFirstName [RequestFirstName]

requestLastName :: String -> BotResult
requestLastName firstName = pureResult (AwaitingLastName {firstName}) [RequestLastName firstName]

requestContactFreq :: String -> String -> BotResult
requestContactFreq firstName lastName =
  pureResult (AwaitingContactFreq {firstName, lastName}) [RequestContactFreq firstName lastName]

saveFriend :: Deps -> WhatsAppNumber -> String -> String -> Int ->  BotResult
saveFriend deps number firstName lastName freq = runExceptT do
  ExceptT $ storeEvents deps.db number [St.addFriendEvent firstName lastName]
  st <- ExceptT $ loadState deps.db number
  let id = fromMaybe (wrap 0) $ maximum $ map fst $ St.friendList st
  ExceptT $ storeEvents deps.db number [St.updateDesiredContactFrequencyEvent id (Just freq)]
  pure $ Idle /\ [AcknowledgeName firstName lastName]

listFriends :: Deps -> WhatsAppNumber -> Unit -> BotResult
listFriends deps number _ = runExceptT do
  state <- ExceptT $ loadState deps.db number
  let friends = sort $ map (snd >>> L.view Friend._name >>> show) $ St.friendList state
  pure $ Idle /\ [FriendsList friends]

compileBot :: Array (String -> Maybe BotResult) -> String -> Maybe BotResult
compileBot attempts input = oneOf $ map (\f -> f input) attempts

handleUserMessage :: Deps -> WhatsAppNumber -> ChatState -> String -> Maybe BotResult
handleUserMessage deps number New = compileBot [
  tryTo ignoreMsg welcomeUser
]
handleUserMessage deps number Idle = compileBot [
  tryTo (isCommand "add") requestFirstName
, tryTo (isCommand "list") (listFriends deps number)
]
handleUserMessage deps number AwaitingFirstName = compileBot [
  tryTo anyMsg requestLastName
]
handleUserMessage deps number (AwaitingLastName {firstName}) = compileBot [
  tryTo anyMsg (requestContactFreq firstName)
]
handleUserMessage deps number (AwaitingContactFreq {firstName, lastName}) = compileBot [
  tryTo (tokens >>> parseDuration) (saveFriend deps number firstName lastName)
]

processMessage :: Deps -> WhatsAppNumber -> ChatState -> String -> BotResult
processMessage deps number state msg =
  fromMaybe (misunderstood state msg) $
  handleUserMessage deps number state msg

inMemoryChatBotStore :: forall id st. (Ord id) => st -> Aff (ChatBotStore Aff id st)
inMemoryChatBotStore newChatState = do
  ref <- liftEffect $ Ref.new M.empty
  pure { storeState: \id st -> liftEffect $ map Right $ Ref.modify_ (M.insert id st) ref
       , retrieveState: \id -> liftEffect $ map Right $ do
           m <- Ref.read ref
           pure $ fromMaybe newChatState $ M.lookup id m
       }

getUserId :: DB -> WhatsAppNumber -> Aff (Either String UserId)
getUserId db waNumber = do
  let newUser = { thirdParty: WhatsApp
                , thirdPartyId: showWhatsAppNumber waNumber
                , name: "UNKNOWN"
                }
  map showError $ DB.upsertUser newUser db

storeState :: DB -> WhatsAppNumber -> ChatState -> Aff (Either String Unit)
storeState db waNumber state = runExceptT do
  uid <- ExceptT $ getUserId db waNumber
  ExceptT $ map showError $ DB.insertState uid (wrap "DunbarChatBot") (encodeJson state) db

retrieveState :: DB -> WhatsAppNumber -> Aff (Either String ChatState)
retrieveState db waNumber = runExceptT do
  uid <- ExceptT $ getUserId db waNumber
  jsonM <- ExceptT $ map showError $ DB.retrieveState uid (wrap "DunbarChatBot") db
  case jsonM of
    (Just json) -> ExceptT $ pure $ decodeJson json
    Nothing -> pure New

postgresChatBotStore :: DB -> (ChatBotStore Aff WhatsAppNumber ChatState)
postgresChatBotStore db = {storeState: storeState db, retrieveState: retrieveState db}

chatBot :: Deps -> ChatBot Aff WhatsAppNumber ChatState BotMessage
chatBot deps = {
  handleMessage: processMessage deps
}

dunbarWhatsAppBot :: DB -> WhatsAppBot Aff
dunbarWhatsAppBot db = whatsAppBot (chatBot deps) (postgresChatBotStore db)
  where deps = {db}
