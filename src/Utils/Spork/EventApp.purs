module Utils.Spork.EventApp where

import Prelude

import Data.Time.Duration (Seconds)
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple (Tuple(..), snd)
import Data.DateTime.Instant (Instant)
import Data.Either (Either(..))
import Data.Foldable (foldr)
import Data.Functor (mapFlipped)

import Effect (Effect)
import Effect.Aff (Aff)

import Spork.App as App
import Spork.Interpreter (basicAff, merge)
import Spork.Html as H

import Utils.Spork.TimerSubscription (runTicker, Sub, tickSub)
import Utils.Alert (alert)
import Utils.AppendStore (AppendStore)
import Utils.Lens (type (:->))
import Utils.Lens as L

data InternalMsg ev =
    LoadEvents
  | ProcessEvents (Array ev)
  | DoNothing

type Transition ev model msg = {
  effects :: Array (Aff msg)
, model :: model
, events :: Array ev
}

type App st ev model msg = {
  render :: model -> H.Html msg
, update :: model -> msg -> Transition ev model msg
, init :: Transition ev model msg
, tick :: Maybe (Tuple (Instant -> msg) Seconds)
, eventStore :: AppendStore ev
, reducer :: ev -> st -> st
, _state :: model :-> st
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

-- TODO apply any events that are returned
mkUpdate :: forall st ev model msg.
            App st ev model msg
         -> model
         -> (Either (InternalMsg ev) msg)
         -> App.Transition Aff model (Either (InternalMsg ev) msg)
mkUpdate {_state, reducer, eventStore, update} m (Right msg) = {effects: allEvents, model: updatedModel}
  where {effects, model, events} = update m msg
        storeEventEffects = mapFlipped events $ \event -> do
          void $ eventStore.append event -- TODO handle error
          pure (Left DoNothing)
        updatedModel = L.over _state (\s -> foldr reducer s events) model
        allEvents = App.batch $ storeEventEffects <> mapmap Right effects
mkUpdate {_state, reducer} model (Left (ProcessEvents events)) = App.purely updatedModel
  where updatedModel = L.over _state (\s -> foldr reducer s events) model
mkUpdate eventApp model (Left LoadEvents) = {effects: App.batch [load], model}
  where load = do
          eventsE <- eventApp.eventStore.retrieveAll
          case eventsE of
            (Right events) -> pure $ Left $ ProcessEvents events
            (Left err) -> pure $ Left DoNothing
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
