module Dunbar.ChatBot where

import Prelude

import Control.Alt (class Alt, (<|>))
import Data.Argonaut.Decode (class DecodeJson, decodeJson, (.:))
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Array (sort, filter, head, catMaybes, take, reverse, sortWith)
import Data.DateTime.Instant (Instant)
import Data.Either (Either(..))
import Data.Foldable (foldr, maximum)
import Data.Int (round)
import Data.Map as M
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (wrap, unwrap)
import Data.String as Str
import Data.Time.Duration (Seconds, Days, convertDuration)
import Data.Traversable (class Traversable, for)
import Data.Tuple (fst, snd)
import Data.Tuple.Nested (type (/\), (/\))
import Dunbar.ChatBot.Parsers (parseDuration, showDuration, parseDate)
import Dunbar.Data.FullName as FN
import Dunbar.Friend as Friend
import Dunbar.State as St
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
import Utils.DateTime (daysToInt, showInstantDate, timeElapsedStr)
import Utils.ExceptT (ExceptT(..), runExceptT, showError, liftEffectRight)
import Utils.Lens as L
import Utils.String (words, unwords, unlines, stripPunctuation)

data ChatState = New
               | Idle
               | AwaitingFirstName
               | AwaitingLastName {firstName :: String}
               | AwaitingContactFreq {firstName :: String, lastName :: String}
               | AwaitingLastSeenDate {id :: St.Id, friend :: Friend.Friend}
               | UpdateFriend {id :: St.Id, friend :: Friend.Friend}

data BotMessage = Intro
                | MainMenu
                | Misunderstood
                | RequestFirstName
                | RequestLastName String
                | RequestContactFreq String String
                | RequestDate
                | AcknowledgeName String String
                | FriendsList (Array String)
                | NextToSee (Array (Friend.Friend /\ Days))
                | JustSeenFriend Friend.Friend
                | SawFriendOnDate Instant Friend.Friend
                | UpdateFriendMenu String
                | DeletedFriend String
                | DateInFuture

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
  encodeJson (AwaitingLastSeenDate r) = encodeJson $ chatStateRecord "AwaitingLastSeenDate" r
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
      "AwaitingLastSeenDate" -> AwaitingLastSeenDate <$> decodeJson stateData
      "UpdateFriend" -> UpdateFriend <$> decodeJson stateData
      "New" -> pure New
      s -> Left $ "Failed to parse state: " <> s

instance toWhatsAppMsgBotMessage :: ToWhatsAppMsg BotMessage where
  toWhatsAppMsg = showBotMessage

showBotMessage :: BotMessage -> String
showBotMessage Intro = """Hi, welcome to John's app for keeping track of your friends. Reply with 'add' to add a friend."""
showBotMessage MainMenu = unlines [
  "Reply with *A* to add a friend."
, "Reply with *N* to see which friends you are overdue being in touch with."
, "Reply with *L* to list your existing friends."
, "Or reply with the name of a friend to update that friend."
]
showBotMessage RequestFirstName = """What's the first name of your friend?"""
showBotMessage (RequestLastName fn) = "What's *" <> fn <> "*'s last name?"
showBotMessage RequestDate = "When did you last see your friend? Enter a date in the format DD/MM/YYYY."
showBotMessage (RequestContactFreq fn ln) = Str.joinWith "\n" $
  [ "How often do you want to be in touch with *" <> fn <> " " <> ln <> "*?"
  , "Respond with an interval (e.g. _every 2 weeks_, _every 3 months_)"]
showBotMessage Misunderstood = """Sorry, I didn't understand what you said."""
showBotMessage (AcknowledgeName fn ln) = "Thanks, you added *" <> fn <> " " <> ln <> "*."
-- TODO show chosen freq
showBotMessage (FriendsList friendNames) =
  "*Your friends:* \n\n" <> Str.joinWith "\n" friendNames
showBotMessage (NextToSee []) = unlines [
  "You are not overdue seeing any of your friends."
, "Make sure you have updated when you last saw them."
]
showBotMessage (NextToSee friendAndOverdueTuples) = unlines $ map row friendAndOverdueTuples
  where row (friend /\ overdueDays) =
          let friendName = show $ L.view Friend._name friend
              durationStr = showDuration (daysToInt overdueDays) in
          friendName <> " (" <> durationStr <> " overdue)"
showBotMessage (JustSeenFriend friend) =
  "Thanks, I've noted that you've just been in touch with *" <> friendName <> "*."
  where friendName = show $ L.view Friend._name friend
showBotMessage (SawFriendOnDate inst friend) =
  "Thanks, I've noted that you were in touch with *" <> friendName <> "* on "
  <> dateStr <> "."
  where friendName = show $ L.view Friend._name friend
        dateStr = showInstantDate inst
showBotMessage (UpdateFriendMenu friendName) = Str.joinWith "\n" $ [
  "Reply with *M* to mark " <> friendName <> " as just seen."
, "Reply with *L* to set the date that you last saw " <> friendName <> "."
, "Reply with *D* to delete " <> friendName <> "."
, "Reply with *B* to go back."
]
showBotMessage (DeletedFriend friendName) = "You deleted " <> friendName <> " ."
showBotMessage DateInFuture = "The date you last saw your friend cannot be in the future."

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


liftF :: forall f a b. Applicative f => (a -> b) -> a -> f b
liftF f a = pure $ f a

liftF2 :: forall f a b c. Applicative f => (a -> b -> c) -> a -> b -> f c
liftF2 f a b = pure $ f a b

anyMsg :: String -> Maybe String
anyMsg = Just

ignoreMsg :: String -> Maybe Unit
ignoreMsg _ = Just unit

isCommand :: String -> String -> Maybe Unit
isCommand cmd input = if (cleaned == cmd) then Just unit else Nothing
  where cleaned = Str.toLower $ Str.trim $ stripPunctuation input

tokens :: String -> Array String
tokens = words >>> map (stripPunctuation >>> Str.toLower)

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

-- TODO create an infix operator for this?
tryTo :: forall a b c m t. (Monad m) => (Traversable t) => (a -> m (t b)) -> (b -> m c) -> (a -> m (t c))
tryTo parser executor s = do
  aM <- parser s -- (f a) -> (a -> m b) -> m (f b)
  for aM executor

pureResult :: ChatState -> Array BotMessage -> ExceptT String Aff BotResult
pureResult st msgs = pure $ st /\ msgs

misunderstood :: ChatState -> String -> BotResult
misunderstood st _ = st /\ [Misunderstood]

welcomeUser :: BotResult
welcomeUser = Idle /\ [Intro]

requestFirstName :: BotResult
requestFirstName = AwaitingFirstName /\ [RequestFirstName]

requestLastName :: String -> BotResult
requestLastName firstName = (AwaitingLastName {firstName}) /\ [RequestLastName firstName]

requestDate :: St.Id -> Friend.Friend -> BotResult
requestDate id friend = AwaitingLastSeenDate {id, friend} /\ [RequestDate]

requestContactFreq :: String -> String -> BotResult
requestContactFreq firstName lastName =
  (AwaitingContactFreq {firstName, lastName}) /\ [RequestContactFreq firstName lastName]

saveFriend :: Deps -> WhatsAppNumber -> String -> String -> Int -> ExceptT String Aff BotResult
saveFriend deps number firstName lastName freq = do
  ExceptT $ storeEvents deps.db number [St.addFriendEvent firstName lastName]
  st <- ExceptT $ loadState deps.db number
  let id = fromMaybe (wrap 0) $ maximum $ map fst $ St.friendList st
  ExceptT $ storeEvents deps.db number [St.updateDesiredContactFrequencyEvent id (Just freq)]
  pure $ Idle /\ [AcknowledgeName firstName lastName, MainMenu]

listFriends :: Deps -> WhatsAppNumber -> ExceptT String Aff BotResult
listFriends deps number = do
  state <- ExceptT $ loadState deps.db number
  let friends = sort $ map (snd >>> L.view Friend._name >>> show) $ St.friendList state
  pure $ Idle /\ [FriendsList friends, MainMenu]

listNextToSee :: St.State -> ExceptT String Aff BotResult
listNextToSee st = do
  n <- liftEffectRight now
  -- let nextToSee = take 10 $ St.overdueContacts n st -- FIXME
  pure $ Idle /\ [NextToSee []]

markFriendSeenDate :: Deps -> WhatsAppNumber -> St.Id -> Friend.Friend -> (Instant -> Friend.Friend -> BotMessage) -> Instant -> ExceptT String Aff BotResult
markFriendSeenDate deps number id friend successMsg dt = do
  n <- liftEffectRight now
  if (dt <= n)
    then do
      ExceptT $ storeEvents deps.db number [St.justSeenEvent id dt]
      pure $ Idle /\ [ successMsg dt friend
                     , MainMenu ]
    else pure $ AwaitingLastSeenDate {id, friend} /\ [DateInFuture, RequestDate]

markFriendAsSeen :: Deps -> WhatsAppNumber -> St.Id -> Friend.Friend -> ExceptT String Aff BotResult
markFriendAsSeen deps number id friend = do
  n <- liftEffectRight now
  markFriendSeenDate deps number id friend (const JustSeenFriend) n

deleteFriend :: Deps -> WhatsAppNumber -> St.Id -> Friend.Friend -> ExceptT String Aff BotResult
deleteFriend deps number id friend = do
  ExceptT $ storeEvents deps.db number [St.deleteFriendEvent id]
  pure $ Idle /\ [ DeletedFriend $ show $ L.view Friend._name friend
                 , MainMenu ]

goBack :: Unit -> BotResult
goBack _ = Idle /\ [MainMenu]

updateFriendMenu :: (St.Id /\ Friend.Friend) -> BotResult
updateFriendMenu (id /\ friend) = UpdateFriend {id, friend} /\ [UpdateFriendMenu $ show $ L.view Friend._name friend]

altF :: forall m f a i. (Monad m) => (Alt f) => (i -> m (f a)) -> (i -> m (f a)) -> i -> m (f a)
altF mFa mFb i = do
  fa <- mFa i
  fb <- mFb i
  pure $ fa <|> fb

infixr 6 altF as <||>

handleUserMessage :: Deps -> WhatsAppNumber -> ChatState -> St.State -> String -> ExceptT String Aff (Maybe BotResult)
handleUserMessage deps number New st =
       tryTo (liftF ignoreMsg) (liftF (const welcomeUser))
handleUserMessage deps number Idle st =
       tryTo (liftF (isCommand "a")) (liftF (const requestFirstName))
  <||> tryTo (liftF (isCommand "n")) (const (listNextToSee st))
  <||> tryTo (liftF (isCommand "l")) (const (listFriends deps number))
  <||> tryTo (liftF (findFriend st)) (liftF updateFriendMenu)
handleUserMessage deps number (UpdateFriend {id, friend}) st =
       tryTo (liftF (isCommand "m")) (const (markFriendAsSeen deps number id friend))
  <||> tryTo (liftF (isCommand "l")) (liftF (const (requestDate id friend)))
  <||> tryTo (liftF (isCommand "d")) (const (deleteFriend deps number id friend))
  <||> tryTo (liftF (isCommand "b")) (liftF goBack)
handleUserMessage deps number AwaitingFirstName st =
       tryTo (liftF anyMsg) (liftF requestLastName)
handleUserMessage deps number (AwaitingLastName {firstName}) st =
       tryTo (liftF anyMsg) (liftF (requestContactFreq firstName))
handleUserMessage deps number (AwaitingContactFreq {firstName, lastName}) st =
       tryTo (liftF (tokens >>> parseDuration)) (saveFriend deps number firstName lastName)
handleUserMessage deps number (AwaitingLastSeenDate {id, friend}) st =
       tryTo (liftF parseDate) (markFriendSeenDate deps number id friend SawFriendOnDate)

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
