module Dunbar.App where

import Prelude
import Data.DateTime.Instant (Instant)
import Data.Tuple (Tuple(..))
import Data.Map as M
import Data.Either (Either(..), either)
import Data.Symbol (SProxy(..))
import Data.List (List(..), (:))
import Data.Maybe (fromMaybe)
import Data.Foldable (foldr)
import Data.Array as Array
import Effect (Effect)
import Effect.Exception.Unsafe (unsafeThrow)
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

import Dunbar.Friend (Friend)
import Dunbar.Friend as Friend
import Dunbar.State (Friendships, Event)
import Dunbar.State as State

type Model = {
  friendships :: Friendships,
  inputs :: Inputs
}

_friendships :: L.Lens' Model Friendships
_friendships = L.prop (SProxy :: SProxy "friendships")

_inputs :: L.Lens' Model Inputs
_inputs = L.prop (SProxy :: SProxy "inputs")

data Msg = Tick Instant
         | UpdateInput (L.Lens' Model String) String
         | AddFriend
         | DoNothing

init :: List Event -> App.Transition Effect Model Msg
init events = App.purely {friendships, inputs}
  where friendships = foldr State.processEvent State.empty events
        inputs = M.empty

renderFriendRow :: forall id. (Show id) => Tuple id Friend -> Tuple String (H.Html Msg)
renderFriendRow (Tuple id friend) = Tuple (show id) $ H.div [] [H.text $ show $ L.view Friend._name friend]

-- TODO render indexed
renderFriendsList :: Model -> H.Html Msg
renderFriendsList model = Keyed.div [] $
                          map renderFriendRow $
                          Array.sortWith sortF $
                          State.friendList model.friendships
  where sortF (Tuple id friend) = L.view Friend._name friend

nonEmptyString :: String -> Either String String
nonEmptyString "" = Left "Can't be empty"
nonEmptyString s = Right s

firstNameInput :: StringInput Model String
firstNameInput = Input.stringInput _inputs "First name" nonEmptyString "firstName"

lastNameInput :: StringInput Model String
lastNameInput = Input.stringInput _inputs "Last name" nonEmptyString "lastName"

submitButton :: forall m. String -> m -> H.Html m
submitButton label msg = H.button [E.onClick (E.always_ msg)] [H.text label]

renderFriendForm :: Model -> H.Html Msg
renderFriendForm model =
  H.div [] [ Input.renderStringInput UpdateInput firstNameInput model
           , Input.renderStringInput UpdateInput lastNameInput model
           , submitButton "Add Friend" AddFriend
           ]

renderSection :: forall msg. String -> H.Html msg -> H.Html msg
renderSection title component = H.div [] [ H.h3 [] [H.text title]
                                         , component]

render :: Model -> H.Html Msg
render model = H.div [] [renderSection "My Friends" $ renderFriendsList model,
                         renderSection "Add Friend" $ renderFriendForm model]

fireStateEvent :: Event -> Model -> App.Transition Effect Model Msg
fireStateEvent event model = {effects, model: updatedModel}
  where effects = App.lift $ do
          storeEvent event
          pure $ DoNothing
        updatedModel = L.over _friendships (State.processEvent event) model

update :: Model → Msg → App.Transition Effect Model Msg
update model (DoNothing) = App.purely model
update model (Tick instant) = App.purely model -- TODO some sort of recalculation
update model (UpdateInput lens val) = App.purely (L.set lens val model)
update model (AddFriend) = fireStateEvent event $
                           Input.clearInput firstNameInput $
                           Input.clearInput lastNameInput $
                           model
  where event = State.addFriendEvent firstName lastName
        firstName = Input.parseStringInputUnsafe firstNameInput model
        lastName = Input.parseStringInputUnsafe lastNameInput model

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

app :: List Event -> App.App Effect Sub Model Msg
app events = {
  render
, update
, subs
, init: init events
}

runApp :: Effect Unit
runApp = do
  refreshEvents
  events <- loadEvents
  inst <- App.makeWithSelector (basicEffect `merge` runSubscriptions) (app events) "#app"
  inst.run
