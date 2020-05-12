module Dunbar.App where

import Prelude
import Data.DateTime.Instant (Instant)
import Data.Tuple (Tuple(..))
import Data.Map as M
import Data.Either (either)
import Data.Symbol (SProxy(..))
import Data.List (List(..), (:))
import Data.Foldable (foldr)
import Data.Array as Array
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Effect (Effect)
import Effect.Exception.Unsafe (unsafeThrow)
import Effect.Now (now)
import Spork.App as App
import Spork.Html as H
import Spork.Html.Events as E
import Spork.Interpreter (merge, basicEffect)
import Spork.Html.Elements.Keyed as Keyed
import Utils.Spork.TimerSubscription (runSubscriptions, tickSub, Sub)
import Utils.Lens as L
import Utils.Components.Input as Input
import Utils.Components.Input (StringInput, Inputs)
import Utils.LocalJsonStorage (load, store) as JsonStorage
import Utils.IdMap as IdMap
import Utils.DateTime as UDT

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

init :: Instant -> List Event -> App.Transition Effect Model Msg
init now events = App.purely {friendships, inputs, now, page}
  where friendships = foldr State.processEvent State.empty events
        inputs = M.empty
        page = Dashboard

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

contactFreqInput :: StringInput Model Int
contactFreqInput = Input.stringInput _inputs "contactFreq"

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
    H.div [] [Input.renderDropdown_ UpdateInput contactFreqInput contactFrequencies model]
  , submitButton "Update" (UpdateFriend id)
  ]
  where title = "Update '" <> show (L.view Friend._name friend) <> "'"
        contactFrequencies = [1, 2, 3, 7, 14, 21, 28, 56, 84]
  -- TODO need to initialise from Friend

render :: Model -> H.Html Msg
render model = case model.page of
  Dashboard -> renderDashboard model
  (UpdateFriendForm id) -> maybe renderFriendNotFound (renderUpdateFriendForm id model) friendM
    where friendM = IdMap.get id model.friendships

-- TODO abstract out
fireStateEvent :: Event -> Msg -> Model -> App.Transition Effect Model Msg
fireStateEvent event msg model = {effects, model: updatedModel}
  where effects = App.lift $ do
          storeEvent event
          pure msg
        updatedModel = L.over _friendships (State.processEvent event) model

-- TODO abstract out
addTimestamp :: Model -> (Maybe Instant -> Msg) -> App.Transition Effect Model Msg
addTimestamp model ctor = {effects, model}
  where effects = App.lift $ do
          nowInst <- now
          pure $ ctor (Just nowInst)

navigate :: Page -> Model -> Model
navigate (Dashboard) model = L.set _page Dashboard model
navigate (UpdateFriendForm id) model =
  L.set _page (UpdateFriendForm id) $
  Input.setInputFromVal contactFreq contactFreqInput $
  model
  where contactFreq = do
          friend <- IdMap.get id model.friendships
          contactFreqDays <- L.view Friend._desiredContactFrequency friend
          pure $ UDT.daysToInt contactFreqDays

update :: Model → Msg → App.Transition Effect Model Msg
update model (DoNothing) = App.purely model
update model (Tick instant) = App.purely $ L.set _now instant model
update model (Navigate page) = App.purely $ navigate page model
update model (UpdateInput lens val) = App.purely $ Input.updateInput lens val model
update model (AddFriend) = fireStateEvent event DoNothing $
                           Input.clearInput firstNameInput $
                           Input.clearInput lastNameInput $
                           model
  where event = State.addFriendEvent firstName lastName
        firstName = Input.parseStringInputUnsafe firstNameInput model
        lastName = Input.parseStringInputUnsafe lastNameInput model
update model (UpdateFriend id) = fireStateEvent event (Navigate Dashboard) model
  where event = State.updateDesiredContactFrequencyEvent id (Just freq)
        freq = Input.parseStringInputUnsafe contactFreqInput model

update model (JustSeen id Nothing) = addTimestamp model (JustSeen id)
update model (JustSeen id (Just timestamp)) = fireStateEvent event DoNothing model
  where event = State.justSeenEvent id timestamp

storageKey :: String
storageKey = "dunbar"

loadEvents :: Effect (List Event)
loadEvents = do
  eventsE <- JsonStorage.load storageKey
  let eventsM = either unsafeThrow identity eventsE
  pure $ fromMaybe Nil eventsM

storeEvents :: List Event -> Effect Unit
storeEvents = JsonStorage.store storageKey

storeEvent :: Event -> Effect Unit
storeEvent event = do
  events <- loadEvents
  storeEvents $ event : events

refreshEvents :: Effect Unit
refreshEvents = do
  events <- loadEvents
  storeEvents events

subs :: Model -> App.Batch Sub Msg
subs model = App.lift (tickSub Tick)

app :: Instant -> List Event -> App.App Effect Sub Model Msg
app currentTime events = {
  render
, update
, subs
, init: init currentTime events
}

runApp :: Effect Unit
runApp = do
  refreshEvents
  events <- loadEvents
  currentTime <- now
  inst <- App.makeWithSelector (basicEffect `merge` runSubscriptions) (app currentTime events) "#app"
  inst.run
