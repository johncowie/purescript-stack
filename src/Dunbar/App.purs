module Dunbar.App where

import Prelude
import Data.DateTime.Instant (Instant)
import Data.Tuple (Tuple(..))
import Data.Map as M
import Data.Either (either)
import Data.Symbol (SProxy(..))
import Data.Array as Array
import Data.Maybe (Maybe(..), maybe)
import Data.Traversable (sequence)
import Data.Newtype (wrap)

import Effect (Effect)
import Effect.Exception (Error)
import Effect.Now (now)

-- import Spork.App as App
import Spork.Html as H
import Spork.Html.Events as E
import Spork.Html.Elements.Keyed as Keyed

import Utils.Spork.EventApp as App
import Utils.Lens as L
import Utils.Components.Input as Input
import Utils.Components.Input (StringInput, Inputs)
import Utils.AppendStore (httpAppendStore, httpSnapshotStore, ApiConfig)
import Utils.IdMap as IdMap
import Utils.DateTime as UDT
import Utils.Async (async)
import Utils.Alert (alert)

import Dunbar.Data.Birthday (Birthday)
import Dunbar.Friend (Friend)
import Dunbar.Friend as Friend
import Dunbar.State (Friendships, Event)
import Dunbar.State as State

data Page = Dashboard | UpdateFriendForm IdMap.Id

type Model = {
  page :: Page,
  friendships :: Friendships,
  inputs :: Inputs,
  now :: Instant,
  eventAppState :: App.EventAppState
}

_friendships :: L.Lens' Model Friendships
_friendships = L.prop (SProxy :: SProxy "friendships")

_inputs :: L.Lens' Model Inputs
_inputs = L.prop (SProxy :: SProxy "inputs")

_now :: L.Lens' Model Instant
_now = L.prop (SProxy :: SProxy "now")

_page :: L.Lens' Model Page
_page = L.prop (SProxy :: SProxy "page")

_eventAppState :: L.Lens' Model App.EventAppState
_eventAppState = L.prop (SProxy :: SProxy "eventAppState")

data Msg = Tick Instant
         | UpdateInput (Input.InputSetter Model) String
         | AddFriend
         | UpdateFriend IdMap.Id
         | DoNothing
         | JustSeen IdMap.Id (Maybe Instant)
         | Navigate Page
         | AlertError String

type Transition = App.Transition Event Model Msg

init :: Instant -> App.Transition Event Model Msg
init now = {effects: [], model, events: []}
  where friendships = State.empty
        inputs = M.empty
        page = Dashboard
        eventAppState = App.emptyState
        model = {friendships, inputs, now, page, eventAppState}

renderFriendRow :: Instant -> Tuple IdMap.Id Friend -> Tuple String (H.Html Msg)
renderFriendRow now (Tuple id friend) =
  Tuple (show id) $
  H.div [] [
    H.div [H.classes ["w-300", "inline-block"]] [H.text $ show $ L.view Friend._name friend]
  , H.div [H.classes ["w-300", "inline-block"]] [H.text $ "Overdue: " <> overdueStr]
  , submitButton "Just contacted" (JustSeen id Nothing)
  , submitButton "Update" (Navigate (UpdateFriendForm id))
  ]
  where timeElapsedStr = maybe "-" UDT.timeElapsedStr $ Friend.timeSinceLastSeen now friend
        freqStr = maybe "-" show $ L.view Friend._desiredContactFrequency friend
        overdueStr = maybe "-" UDT.timeElapsedStr $ Friend.overdueContact now friend

-- TODO render indexed
renderFriendsList :: Model -> H.Html Msg
renderFriendsList model = Keyed.div [] $
                          map (renderFriendRow currentTime) $
                          Array.sortWith sortF $
                          State.friendList model.friendships
  where sortF (Tuple id friend) = L.view Friend._name friend
        currentTime = L.view _now model

firstNameInput :: StringInput Model String
firstNameInput = Input.stringInput _inputs "firstName"

lastNameInput :: StringInput Model String
lastNameInput = Input.stringInput _inputs "lastName"

contactFreqInput :: IdMap.Id -> StringInput Model (Maybe Int)
contactFreqInput id = Input.stringInput _inputs $ "contactFreq" <> show id

notesInput :: IdMap.Id -> StringInput Model (Maybe String)
notesInput id = Input.stringInput _inputs $ "notes" <> show id

birthdayInput :: IdMap.Id -> StringInput Model (Maybe Birthday)
birthdayInput id = Input.stringInput _inputs $ "birthday" <> show id

submitButton :: forall m. String -> m -> H.Html m
submitButton label msg = H.button [E.onClick (E.always_ msg)] [H.text label]

renderFriendForm :: Model -> H.Html Msg
renderFriendForm model =
  H.div [] [ Input.renderStringInput UpdateInput firstNameInput "first name" model
           , Input.renderStringInput UpdateInput lastNameInput "last name" model
           , submitButton "Add Friend" AddFriend
           ]

renderSection :: forall msg. String -> H.Html msg -> H.Html msg
renderSection title component = H.div [] [ H.h3 [] [H.text title]
                                         , component]

renderDashboard :: Model -> H.Html Msg
renderDashboard model = H.div [] [
  renderSection "Add Friend" $ renderFriendForm model,
  renderSection "My Friends" $ renderFriendsList model
]

renderFriendNotFound :: H.Html Msg
renderFriendNotFound = H.div [] [H.h3 [] [H.text "Friend 404"]]

renderUpdateFriendForm :: IdMap.Id -> Model -> Friend -> H.Html Msg
renderUpdateFriendForm id model friend = renderSection title $
  H.div [] [
    H.div [] [Input.renderDropdown UpdateInput (contactFreqInput id) contactFrequencies model]
  , H.div [] [ H.text "Birthday: "
             , Input.renderStringInput UpdateInput (birthdayInput id) "birthday" model] -- TODO
  , H.div [] [Input.renderTextArea UpdateInput (notesInput id) "notes" model]
  , submitButton "Update" (UpdateFriend id)
  , H.div [] $ [H.a [E.onClick (E.always_ (Navigate Dashboard)), H.href ""] [H.text "Back to dashboard"]]
  ]
  where title = "Update '" <> show (L.view Friend._name friend) <> "'"
        contactFrequencies = [ Tuple "-" Nothing
                             , Tuple "Every day" (Just 1)
                             , Tuple "Every 2 days" (Just 2)
                             , Tuple "Every 3 days" (Just 3)
                             , Tuple "Every week" (Just 7)
                             , Tuple "Every 2 weeks" (Just 14)
                             , Tuple "Every 3 weeks" (Just 21)
                             , Tuple "Every month" (Just 30)
                             , Tuple "Every 2 months" (Just 60)
                             , Tuple "Every 3 months" (Just 90)]

render :: Model -> H.Html Msg
render model = case model.page of
  Dashboard -> renderDashboard model
  (UpdateFriendForm id) -> maybe renderFriendNotFound (renderUpdateFriendForm id model) friendM
    where friendM = IdMap.get id model.friendships

alertError :: Model -> String -> Transition
alertError model s = {effects, model, events: []}
  where effects = [pure $ AlertError s]

-- TODO abstract out
addTimestamp :: Model -> (Maybe Instant -> Msg) -> Transition
addTimestamp model ctor = {effects: [effect], model, events: []}
  where effect = async $ do
          nowInst <- now
          pure $ ctor (Just nowInst)

navigate :: Page -> Model -> Model
navigate (Dashboard) model = L.set _page Dashboard model
navigate (UpdateFriendForm id) model =
  L.set _page (UpdateFriendForm id) $
  Input.setInputFromVal (Just contactFreq) (contactFreqInput id) $
  Input.setInputFromVal (Just notes) (notesInput id) $
  Input.setInputFromVal (Just birthday) (birthdayInput id) $
  model
  where contactFreq = do
          friend <- IdMap.get id model.friendships
          contactFreqDays <- L.view Friend._desiredContactFrequency friend
          pure $ UDT.daysToInt contactFreqDays
        notes = do
          friend <- IdMap.get id model.friendships
          L.view Friend._notes friend
        birthday = do
          friend <- IdMap.get id model.friendships
          L.view Friend._birthday friend

update :: Model → Msg → Transition
update model (DoNothing) = App.purely model
update model (Tick instant) = App.purely $ L.set _now instant model
update model (Navigate page) = App.purely $ navigate page model
update model (UpdateInput lens val) = App.purely $ Input.updateInput lens val model
update model (AddFriend) = either (alertError model) (\event -> {effects: [], model: clearedInputs, events: [event]}) eventE
  where eventE = State.addFriendEvent <$>
                 Input.parseStringInput firstNameInput model <*>
                 Input.parseStringInput lastNameInput model
        clearedInputs = Input.clearInput firstNameInput $
                        Input.clearInput lastNameInput $ model
update model (UpdateFriend id) = either (alertError model)
                                  { effects: [pure $ Navigate Dashboard]
                                  , model: clearedInputs
                                  , events: _ }
                                  eventsE
  where update1 = State.updateDesiredContactFrequencyEvent id <$>
                  Input.parseStringInput (contactFreqInput id) model
        update2 = State.updateNotesEvent id <$>
                  Input.parseStringInput (notesInput id) model
        update3 = State.updateBirthdayEvent id <$>
                  Input.parseStringInput (birthdayInput id) model
        clearedInputs = Input.clearInput (contactFreqInput id) $
                        Input.clearInput (notesInput id) $
                        Input.clearInput (birthdayInput id) $ model
        eventsE = sequence [update1, update2, update3]
update model (JustSeen id Nothing) = addTimestamp model (JustSeen id)
update model (JustSeen id (Just timestamp)) = {effects: [], model, events: [event]}
  where event = State.justSeenEvent id timestamp
update model (AlertError err) = {effects: [effect], model, events: []}
  where effect = do
          async $ alert err
          pure DoNothing

type AppConfig = {
  apiConfig :: ApiConfig
}

app :: AppConfig -> Instant -> App.App Friendships Event Model Msg
app config currentTime = {
  render
, update
, init: init currentTime
, tick: Just (Tuple Tick (wrap 5000.0))
, _state: _friendships
, _eventAppState
, reducer: State.processEvent
, eventStore: httpAppendStore config.apiConfig "dunbar"
, snapshotStore: httpSnapshotStore config.apiConfig "snapshot"
}

affErrorHandler :: Error -> Effect Unit
affErrorHandler err = alert (show err)

mkConfig :: ApiConfig -> AppConfig
mkConfig = {apiConfig: _}

runApp :: AppConfig -> Effect Unit
runApp config = do
  currentTime <- now
  inst <- App.makeWithSelector (app config currentTime) "#app"
  inst.run
