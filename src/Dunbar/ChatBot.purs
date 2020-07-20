module Dunbar.ChatBot where

import Prelude

import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Data.Argonaut.Decode (class DecodeJson, decodeJson, (.:))
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Array (sort, filter, head)
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
import Dunbar.Data.FullName as FN
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Now (now)
import Effect.Ref as Ref
import Server.ChatBot (ChatBot, ChatBotStore)
import Server.ChatBot.WhatsApp (WhatsAppBot, whatsAppBot, class ToWhatsAppMsg)
import Server.DB (DB)
import Server.DB as DB
import Server.Domain (OAuthProvider(WhatsApp), UserId)
import Twilio.WhatsApp (WhatsAppNumber, showWhatsAppNumber)
import Utils.ExceptT (ExceptT(..), runExceptT, showError, liftEffectRight)
import Utils.Lens as L

data ChatState = New
               | Idle
               | AwaitingFirstName
               | AwaitingLastName {firstName :: String}
               | AwaitingContactFreq {firstName :: String, lastName :: String}
               | UpdateFriend {id :: St.Id, friend :: Friend.Friend}

data BotMessage = Intro
                | MainMenu
                | Misunderstood
                | RequestFirstName
                | RequestLastName String
                | RequestContactFreq String String
                | AcknowledgeName String String
                | FriendsList (Array String)
                | JustSeenFriend String
                | UpdateFriendMenu String
                | DeletedFriend String

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
  encodeJson (UpdateFriend r) = encodeJson $ chatStateRecord "UpdateFriend" r
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
      "UpdateFriend" -> UpdateFriend <$> decodeJson stateData
      "New" -> pure New
      s -> Left $ "Failed to parse state: " <> s

instance toWhatsAppMsgBotMessage :: ToWhatsAppMsg BotMessage where
  toWhatsAppMsg = showBotMessage

unwords :: Array String -> String
unwords = Str.joinWith "\n"

showBotMessage :: BotMessage -> String
showBotMessage Intro = """Hi, welcome to John's app for keeping track of your friends. Reply with 'add' to add a friend."""
showBotMessage MainMenu = unwords [
  "Reply with *A* to add a friend."
, "Reply with *L* to list your existing friends."
, "Or reply with the name of a friend to update that friend."
]
showBotMessage RequestFirstName = """What's the first name of your friend?"""
showBotMessage (RequestLastName fn) = "What's *" <> fn <> "*'s last name?"
showBotMessage (RequestContactFreq fn ln) = Str.joinWith "\n" $
  [ "How often do you want to be in touch with *" <> fn <> " " <> ln <> "*?"
  , "Respond with an interval (e.g. _every 2 weeks_, _every 3 months_)"]
showBotMessage Misunderstood = """Sorry, I didn't understand what you said."""
showBotMessage (AcknowledgeName fn ln) = "Thanks, you added *" <> fn <> " " <> ln <> "*."
-- TODO show chosen freq
showBotMessage (FriendsList friendNames) =
  "*Your friends:* \n\n" <> Str.joinWith "\n" friendNames
showBotMessage (JustSeenFriend friendName) =
  "Thanks, I've noted that you've just been in touch with *" <> friendName <> "*."
showBotMessage (UpdateFriendMenu friendName) = Str.joinWith "\n" $ [
  "Reply with *M* to mark " <> friendName <> " as just seen."
, "Reply with *D* to delete " <> friendName <> "."
, "Reply with *B* to go back."
]
showBotMessage (DeletedFriend friendName) = "You deleted " <> friendName <> " ."

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

liftF :: forall f a b. Applicative f => (a -> b) -> a -> f b
liftF f a = pure $ f a

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

parseAmountWord :: String -> Maybe Int
parseAmountWord "one" = Just 1
parseAmountWord "two" = Just 2
parseAmountWord "three" = Just 3
parseAmountWord "four" = Just 4
parseAmountWord "five" = Just 5
parseAmountWord "six" = Just 6
parseAmountWord "seven" = Just 7
parseAmountWord "eight" = Just 8
parseAmountWord "nine" = Just 9
parseAmountWord "ten" = Just 10
parseAmountWord _ = Nothing

parseAmount :: String -> Maybe Int
parseAmount s = do
  i <- Int.fromString s
  if (i > 0) then Just i else parseAmountWord s

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
parseDuration ["every", unit] = parseTimeUnit unit
parseDuration [amount, unit] = do
  a <- parseAmount amount
  u <- parseTimeUnit unit
  pure $ a * u
parseDuration ["every", amount, unit] = parseDuration [amount, unit]
parseDuration _ = Nothing

nameMatch :: String -> (St.Id /\ Friend.Friend) -> Boolean
nameMatch input (id /\ friend) = case tokens input of
  [singleWord] -> singleWord == firstNameToken || singleWord == lastNameToken
  [f, l] -> f == firstNameToken && l == lastNameToken
  _ -> false
  where firstNameToken = Str.toLower $ L.view (Friend._name >>> FN._firstName) friend
        lastNameToken = Str.toLower $ L.view (Friend._name >>> FN._lastName) friend

findFriend :: St.State -> String -> Maybe (St.Id /\ Friend.Friend)
findFriend st input = (St.friendList >>> filter (nameMatch input) >>> head) st

type BotResult = (ChatState /\ Array BotMessage)

tryTo :: forall a m. (Monad m) => (St.State -> String -> Maybe a) -> (a -> m BotResult) -> (St.State -> String -> m (Maybe BotResult))
tryTo parser executor st s = do
  let vM = parser st s
  case vM of
    (Just v) -> do
      r <- executor v
      pure $ Just r
    Nothing -> pure $ Nothing

pureResult :: ChatState -> Array BotMessage -> ExceptT String Aff BotResult
pureResult st msgs = pure $ st /\ msgs

misunderstood :: ChatState -> String -> BotResult
misunderstood st _ = st /\ [Misunderstood]

welcomeUser :: Unit -> BotResult
welcomeUser _ = Idle /\ [Intro]

requestFirstName :: Unit -> BotResult
requestFirstName _ = AwaitingFirstName /\ [RequestFirstName]

requestLastName :: String -> BotResult
requestLastName firstName = (AwaitingLastName {firstName}) /\ [RequestLastName firstName]

requestContactFreq :: String -> String -> BotResult
requestContactFreq firstName lastName =
  (AwaitingContactFreq {firstName, lastName}) /\ [RequestContactFreq firstName lastName]

saveFriend :: Deps -> WhatsAppNumber -> String -> String -> Int -> ExceptT String Aff BotResult
saveFriend deps number firstName lastName freq = do
  ExceptT $ storeEvents deps.db number [St.addFriendEvent firstName lastName]
  st <- ExceptT $ loadState deps.db number
  let id = fromMaybe (wrap 0) $ maximum $ map fst $ St.friendList st
  ExceptT $ storeEvents deps.db number [St.updateDesiredContactFrequencyEvent id (Just freq)]
  pure $ Idle /\ [AcknowledgeName firstName lastName]

listFriends :: Deps -> WhatsAppNumber -> Unit -> ExceptT String Aff BotResult
listFriends deps number _ = do
  state <- ExceptT $ loadState deps.db number
  let friends = sort $ map (snd >>> L.view Friend._name >>> show) $ St.friendList state
  pure $ Idle /\ [FriendsList friends, MainMenu]

markFriendAsSeen :: Deps -> WhatsAppNumber -> (St.Id /\ Friend.Friend) -> ExceptT String Aff BotResult
markFriendAsSeen deps number (id /\ friend) = do
  n <- liftEffectRight now
  ExceptT $ storeEvents deps.db number [St.justSeenEvent id n]
  pure $ Idle /\ [ JustSeenFriend $ show $ L.view Friend._name friend
                 , MainMenu ]

deleteFriend :: Deps -> WhatsAppNumber -> (St.Id /\ Friend.Friend) -> ExceptT String Aff BotResult
deleteFriend deps number (id /\ friend) = do
  ExceptT $ storeEvents deps.db number [St.deleteFriendEvent id]
  pure $ Idle /\ [ DeletedFriend $ show $ L.view Friend._name friend
                 , MainMenu ]

goBack :: Unit -> BotResult
goBack _ = Idle /\ [MainMenu]

updateFriendMenu :: (St.Id /\ Friend.Friend) -> BotResult
updateFriendMenu (id /\ friend) = UpdateFriend {id, friend} /\ [UpdateFriendMenu $ show $ L.view Friend._name friend]

compileBot :: forall m. (Monad m) => Array (St.State -> String -> m (Maybe BotResult)) -> St.State -> String -> m (Maybe BotResult)
compileBot attempts st input = runMaybeT $ oneOf $ map (\f -> MaybeT $ f st input) attempts

handleUserMessage :: Deps -> WhatsAppNumber -> ChatState -> St.State -> String -> ExceptT String Aff (Maybe BotResult)
handleUserMessage deps number New = compileBot [
  tryTo (const ignoreMsg) (liftF welcomeUser)
]
handleUserMessage deps number Idle = compileBot [
  tryTo (const (isCommand "a")) (liftF requestFirstName)
, tryTo (const (isCommand "l")) (listFriends deps number)
, tryTo findFriend (liftF updateFriendMenu)
]
handleUserMessage deps number (UpdateFriend {id, friend}) = compileBot [
  tryTo (const (isCommand "m")) (const (markFriendAsSeen deps number (id /\ friend)))
, tryTo (const (isCommand "d")) (const (deleteFriend deps number (id /\ friend)))
, tryTo (const (isCommand "b")) (liftF goBack)
]
handleUserMessage deps number AwaitingFirstName = compileBot [
  tryTo (const anyMsg) (liftF requestLastName)
]
handleUserMessage deps number (AwaitingLastName {firstName}) = compileBot [
  tryTo (const anyMsg) (liftF (requestContactFreq firstName))
]
handleUserMessage deps number (AwaitingContactFreq {firstName, lastName}) = compileBot [
  tryTo (const (tokens >>> parseDuration)) (saveFriend deps number firstName lastName)
]

processMessage :: Deps -> WhatsAppNumber -> ChatState -> String -> Aff (Either String BotResult)
processMessage deps number chatState msg = runExceptT do
  state <- ExceptT $ loadState deps.db number
  rM <- handleUserMessage deps number chatState state msg
  pure $ fromMaybe (misunderstood chatState msg) $ rM

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
