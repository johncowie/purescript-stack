module Utils.Spork.EventApp
( EventAppState
, makeWithSelector
, App
, Transition
, InternalMsg
, purely
, emptyState
)
where

import Prelude

import Data.Time.Duration (Seconds)
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple (Tuple(..), snd)
import Data.DateTime.Instant (Instant)
import Data.Either (Either(..))
import Data.Foldable (foldr)
import Data.Functor (mapFlipped)
import Data.Newtype (class Newtype)
import Data.Map as M

import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)

import Spork.App as App
import Spork.Interpreter (basicAff, merge)
import Spork.Html as H

import Utils.Spork.TimerSubscription (runTicker, Sub, tickSub)
import Utils.Alert (alert)
import Utils.AppendStore (AppendStore, SnapshotStore)
import Utils.Lens (type (:->))
import Utils.Lens as L
import Utils.Components.Input (Inputs)

data InternalMsg ev =
    LoadEvents
  | ProcessEvents (Array ev)
  | DoNothing
  | AlertError String

type Transition ev model msg = {
  effects :: Array (Aff msg)
, model :: model
, events :: Array ev
}

newtype EventAppState = EventAppState {
  inputs :: Inputs
}

emptyState :: EventAppState
emptyState = EventAppState {inputs: M.empty}

derive instance newtypeEventAppState :: Newtype EventAppState _

type App st ev model msg = {
  render :: model -> H.Html msg
, update :: model -> msg -> Transition ev model msg
, init :: Transition ev model msg
, tick :: Maybe (Tuple (Instant -> msg) Seconds)
, eventStore :: AppendStore Int ev -- TODO combine these
, snapshotStore :: SnapshotStore st
, reducer :: ev -> st -> st
, _state :: model :-> st
, _eventAppState :: model :-> EventAppState
}

purely :: forall ev model msg. model -> Transition ev model msg
purely model = {effects: [], events: [], model}

mapmap :: forall f g a b. (Functor f) => (Functor g) => (a -> b) -> f (g a) -> f (g b)
mapmap f = map (map f)

mkInit :: forall st ev model msg. App st ev model msg -> App.Transition Aff model (Either (InternalMsg ev) msg)
mkInit eventApp = {effects: App.batch allEffects, model}
  where {effects, model} = eventApp.init
        allEffects = [pure (Left LoadEvents)] <> mapmap Right effects

mkRender :: forall st ev model msg. App st ev model msg -> model -> H.Html (Either (InternalMsg ev) msg)
mkRender eventApp = map Right <<< eventApp.render

mkSubs :: forall st ev model msg. App st ev model msg -> model -> App.Batch Sub (Either (InternalMsg ev) msg)
mkSubs eventApp model = maybe (App.batch []) App.lift tickSubM
  where tickSubM = case eventApp.tick of
                      (Just (Tuple tickMsg _)) -> Just $ tickSub (\i -> Right $ tickMsg i)
                      _ -> Nothing

alertError :: forall ev msg. String -> Aff (Either (InternalMsg ev) msg)
alertError err = do
  liftEffect $ alert err
  pure (Left DoNothing)

-- TODO apply any events that are returned
mkUpdate :: forall st ev model msg.
            App st ev model msg
         -> model
         -> (Either (InternalMsg ev) msg)
         -> App.Transition Aff model (Either (InternalMsg ev) msg)
mkUpdate {_state, reducer, eventStore, update} m (Right msg) = {effects: allEvents, model: updatedModel}
  where {effects, model, events} = update m msg
        storeEventEffects = mapFlipped events $ \event -> do
          result <- eventStore.append event -- TODO sequence errors
          case result of
            (Left err) -> pure $ Left $ AlertError (show err)
            _  -> pure $ Left DoNothing
        updatedModel = L.over _state (\s -> foldr reducer s events) model
        allEvents = App.batch $ storeEventEffects <> mapmap Right effects
mkUpdate {_state, reducer} model (Left (ProcessEvents events)) = App.purely updatedModel
  where updatedModel = L.over _state (\s -> foldr reducer s events) model
mkUpdate eventApp model (Left LoadEvents) = {effects: App.batch [load], model}
  where load = do
          eventsE <- eventApp.eventStore.retrieveAll
          case eventsE of
            (Right events) -> pure $ Left $ ProcessEvents $ map _.event events -- TODO keep track of max event ID somewhere
            (Left err) -> pure $ Left $ AlertError $ show err
mkUpdate eventApp model (Left (AlertError e)) = {effects: App.batch [alertError e], model: model}
mkUpdate eventapp model (Left DoNothing) = App.purely model

toApp :: forall st ev model msg. App st ev model msg -> App.App Aff Sub model (Either (InternalMsg ev) msg)
toApp eventApp = {render, update, subs, init}
  where update = mkUpdate eventApp
        init = mkInit eventApp
        render = mkRender eventApp
        subs = mkSubs eventApp

affErrorHandler :: forall err. (Show err) => err ->  Effect Unit
affErrorHandler err = alert (show err)

makeWithSelector :: forall st ev model msg.
           App st ev model msg
        -> String
        -> Effect (App.AppInstance model (Either (InternalMsg ev) msg))
makeWithSelector eventApp selector =
  App.makeWithSelector interpreter app selector
  where interpreter = basicAff affErrorHandler `merge` (runTicker tickSecs)
        app = toApp eventApp
        tickSecs = snd <$> eventApp.tick


{- Snapshot stuff
   [ ] Support optional query param in API to retrieve events from a certain point
   [ ] Store event number in state
   [ ] Store snapshot periodically
   [ ] load snapshot when loading events
   [ ] load events after snapshot and use snapshot as base to layer events on

-}

{-

  Input stuff

  input :: forall model a. String -> StringInput model a
  input = unsafeThrow ""

  input_ :: forall model a. String -> StringInput model a
  input_ = unsafeThrow ""

  hmmm how's this msg business going to work - configure InputSet in app?
  renderInput :: forall model a. StringInput model a -> String -> model -> H.Html msg

-}

{-
  Thoughts

  EventApp is a special type of app that supports this mechanics for
  storing events, loading events and recreating a state
  - (and also the inputs mechanism as maybe an afterthought)

  ## Storing events
  ## A new version of the App transition could also have an events key
  i.e. {effects, model, events}
  ## Might be nice to simplify the return type of effects to be Aff [Msg]

  ## Specifying tick
  - there should be a Maybe Seconds option for specifying a tick interval, along with a msg
-}