module Dunbar.App where

import Prelude
import Data.DateTime.Instant (Instant)
import Data.Tuple (Tuple(..))
import Data.Map as M
import Data.Either (either)
import Data.Symbol (SProxy(..))
import Data.Array as Array
import Data.Maybe (Maybe(..), maybe)
import Data.Foldable (foldr)
import Data.Traversable (for, sequence)

import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Exception (Error)
import Effect.Now (now)

import Spork.App as App
import Spork.Html as H
import Spork.Html.Events as E
import Spork.Interpreter (merge, basicAff)
import Spork.Html.Elements.Keyed as Keyed

import Utils.Spork.TimerSubscription (runSubscriptions, tickSub, Sub)
import Utils.Lens as L
import Utils.Components.Input as Input
import Utils.Components.Input (StringInput, Inputs)
import Utils.AppendStore (AppendStore, httpAppendStore)
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
  now :: Instant
}

_friendships :: L.Lens' Model Friendships
_friendships = L.prop (SProxy :: SProxy "friendships")

_inputs :: L.Lens' Model Inputs
_inputs = L.prop (SProxy :: SProxy "inputs")

_now :: L.Lens' Model Instant
_now = L.prop (SProxy :: SProxy "now")

_page :: L.Lens' Model Page
_page = L.prop (SProxy :: SProxy "page")

data Msg = Tick Instant
         | UpdateInput (Input.InputSetter Model) String
         | AddFriend
         | UpdateFriend IdMap.Id
         | DoNothing
         | JustSeen IdMap.Id (Maybe Instant)
         | Navigate Page
         | LoadEvents
         | LoadedEvents (Array Event)
         | AlertError String

init :: Instant -> App.Transition Aff Model Msg
init now = {effects, model}
  where friendships = State.empty
        inputs = M.empty
        page = Dashboard
        model = {friendships, inputs, now, page}
        effects = App.lift $ pure $ LoadEvents

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

contactFreqInput :: StringInput Model (Maybe Int)
contactFreqInput = Input.stringInput _inputs "contactFreq"

notesInput :: StringInput Model (Maybe String)
notesInput = Input.stringInput _inputs "notes"

birthdayInput :: StringInput Model (Maybe Birthday)
birthdayInput = Input.stringInput _inputs "birthday"

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
    H.div [] [Input.renderDropdown UpdateInput contactFreqInput contactFrequencies model]
  , H.div [] [ H.text "Birthday: "
             , Input.renderStringInput UpdateInput birthdayInput "birthday" model] -- TODO
  , H.div [] [Input.renderTextArea UpdateInput notesInput "notes" model]
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

-- TODO abstract out
fireStateEvent :: Msg -> Model -> Event -> App.Transition Aff Model Msg
fireStateEvent msg model event = {effects, model: updatedModel}
  where effects = App.lift $ do
          void $ store.append event
          pure msg
        updatedModel = L.over _friendships (State.processEvent event) model

fireStateEvents :: Msg -> Model -> Array Event -> App.Transition Aff Model Msg
fireStateEvents msg model events = {effects, model: updatedModel}
  where effects = App.lift $ do
          void $ for events store.append
          pure msg
        updatedModel = L.over _friendships (\f -> foldr State.processEvent f events) model

alertError :: Model -> String -> App.Transition Aff Model Msg
alertError model s = {effects, model}
  where effects = pure $ AlertError s

-- TODO abstract out
addTimestamp :: Model -> (Maybe Instant -> Msg) -> App.Transition Aff Model Msg
addTimestamp model ctor = {effects, model}
  where effects = App.lift $ async $ do
          nowInst <- now
          pure $ ctor (Just nowInst)

navigate :: Page -> Model -> Model
navigate (Dashboard) model = L.set _page Dashboard model
navigate (UpdateFriendForm id) model =
  L.set _page (UpdateFriendForm id) $
  Input.setInputFromVal (Just contactFreq) contactFreqInput $
  Input.setInputFromVal (Just notes) notesInput $
  Input.setInputFromVal (Just birthday) birthdayInput $
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

update :: Model → Msg → App.Transition Aff Model Msg
update model (DoNothing) = App.purely model
update model (LoadEvents) = {effects, model}
  where effects = App.lift $ do
          eventsE <- store.retrieveAll
          pure $ either (AlertError <<< show) LoadedEvents eventsE
update model (LoadedEvents events) = App.purely updatedModel
  where updatedModel = L.over _friendships (\s -> foldr State.processEvent s events) model
update model (Tick instant) = App.purely $ L.set _now instant model
update model (Navigate page) = App.purely $ navigate page model
update model (UpdateInput lens val) = App.purely $ Input.updateInput lens val model
update model (AddFriend) = either (alertError model) (fireStateEvent DoNothing clearedInputs) event
  where event = State.addFriendEvent <$>
                Input.parseStringInput firstNameInput model <*>
                Input.parseStringInput lastNameInput model
        clearedInputs = Input.clearInput firstNameInput $
                        Input.clearInput lastNameInput $ model
update model (UpdateFriend id) = either (alertError model) (fireStateEvents (Navigate Dashboard) clearedInputs) events
  where update1 = State.updateDesiredContactFrequencyEvent id <$>
                  Input.parseStringInput contactFreqInput model
        update2 = State.updateNotesEvent id <$>
                  Input.parseStringInput notesInput model
        update3 = State.updateBirthdayEvent id <$>
                  Input.parseStringInput birthdayInput model
        clearedInputs = Input.clearInput contactFreqInput $
                        Input.clearInput notesInput $
                        Input.clearInput birthdayInput $ model
        events = sequence [update1, update2, update3]
update model (JustSeen id Nothing) = addTimestamp model (JustSeen id)
update model (JustSeen id (Just timestamp)) = fireStateEvent DoNothing model event
  where event = State.justSeenEvent id timestamp
update model (AlertError err) = {effects, model}
  where effects = App.lift do
          async $ alert err
          pure DoNothing

store :: AppendStore Event
store = httpAppendStore "dunbar"

subs :: Model -> App.Batch Sub Msg
subs model = App.lift (tickSub Tick)

app :: Instant -> App.App Aff Sub Model Msg
app currentTime = {
  render
, update
, subs
, init: init currentTime
}

affErrorHandler :: Error -> Effect Unit
affErrorHandler err = alert (show err)

runApp :: Effect Unit
runApp = do
  currentTime <- now
  inst <- App.makeWithSelector (basicAff affErrorHandler `merge` runSubscriptions) (app currentTime) "#app"
  inst.run
