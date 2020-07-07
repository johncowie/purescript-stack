module Utils.Spork.EventApp
( EventAppState
, makeWithSelector
, App
, Transition
, InternalMsg
, MsgResult
, purely
, emptyState
)
where

import Prelude

import Control.Monad.Except.Trans (ExceptT(..), runExceptT)
import Data.DateTime.Instant (Instant)
import Data.Either (Either(..))
import Data.Foldable (foldM, foldr)
import Data.Map as M
import Data.Maybe (Maybe(..), maybe, fromMaybe)
import Data.Newtype (class Newtype)
import Data.Time.Duration (Seconds)
import Data.Tuple (Tuple(..), snd)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Spork.App as App
import Spork.Html as H
import Spork.Interpreter (basicAff, merge)
import Utils.Alert (alert)
import Utils.AppendStore (AppendStore, SnapshotStore)
import Utils.Components.Input (Inputs)
import Utils.Lens (type (:->))
import Utils.Lens as L
import Utils.Spork.TimerSubscription (runTicker, Sub, tickSub)

data InternalMsg st ev =
    LoadEvents
  | ProcessEvents (Maybe st) (Array ev)
  | SaveEvents (Array ev)
  | DoNothing
  | AlertError String
  | ResetState st

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

data MsgResult st ev msg = Internal (InternalMsg st ev) | External msg

purely :: forall ev model msg. model -> Transition ev model msg
purely model = {effects: [], events: [], model}

mapmap :: forall f g a b. (Functor f) => (Functor g) => (a -> b) -> f (g a) -> f (g b)
mapmap f = map (map f)

mkInit :: forall st ev model msg. App st ev model msg -> App.Transition Aff model (MsgResult st ev msg)
mkInit eventApp = {effects: App.batch allEffects, model}
  where {effects, model} = eventApp.init
        allEffects = [pure (Internal LoadEvents)] <> mapmap External effects

mkRender :: forall st ev model msg. App st ev model msg -> model -> H.Html (MsgResult st ev msg)
mkRender eventApp = map External <<< eventApp.render

mkSubs :: forall st ev model msg. App st ev model msg -> model -> App.Batch Sub (MsgResult st ev msg)
mkSubs eventApp model = maybe (App.batch []) App.lift tickSubM
  where tickSubM = case eventApp.tick of
                      (Just (Tuple tickMsg _)) -> Just $ tickSub (\i -> External $ tickMsg i)
                      _ -> Nothing

alertError :: forall st ev msg. String -> Aff (MsgResult st ev msg)
alertError err = do
  liftEffect $ alert err
  pure (Internal DoNothing)

alertErrorShow :: forall err st ev msg. (Show err) => Either err (MsgResult st ev msg) -> MsgResult st ev msg
alertErrorShow (Left err) = Internal $ AlertError (show err)
alertErrorShow (Right res) = res

mapLeft :: forall a b c. (a -> c) -> Either a b -> Either c b
mapLeft f (Left e) = Left $ f e
mapLeft f (Right v) = Right v

loadEvents :: forall ev st msg. AppendStore Int ev -> SnapshotStore st -> Aff (MsgResult st ev msg)
loadEvents eventStore snapshotStore = alertErrorShow <$> runExceptT do
  snapshotM <- ExceptT snapshotStore.retrieveLatestSnapshot
  let stateM = _.state <$> snapshotM
  let lastEventId = fromMaybe 0 $ _.upToEvent <$> snapshotM
  events <- ExceptT $ eventStore.retrieveAfter lastEventId
  pure (Internal (ProcessEvents stateM (map _.event events)))

applyEvents :: forall ev st. (ev -> st -> st) -> Maybe st -> Array ev -> st -> st
applyEvents reducer snapshotStateM events state =
  foldr reducer initialState events
  where initialState = fromMaybe state snapshotStateM

-- saveEvents :: AppendStore Int ev -> SnapshotStore st -> st -> ev
saveEvent :: forall st ev.
             AppendStore Int ev
          -> SnapshotStore st
          -> (ev -> st -> st)
          -> st
          -> ev
          -> ExceptT String Aff st
saveEvent eventStore snapshotStore reducer state event = do
  eventId <- ExceptT $ mapLeft show <$> eventStore.append event
  let updatedState = reducer event state
  if eventId `mod` 10 == 0
    then ExceptT $ mapLeft show <$> snapshotStore.saveSnapshot {state: updatedState, upToEvent: eventId}
    else pure unit
  pure updatedState

-- TODO pull event saving out into separate function
-- TODO whenever saving an event, also save a snapshot
internalUpdate :: forall st ev model msg.
                  App st ev model msg
               -> model
               -> InternalMsg st ev
               -> App.Transition Aff model (MsgResult st ev msg)
internalUpdate {_state, reducer, eventStore, snapshotStore} model (SaveEvents events) =
  {effects: App.batch [effect], model}
  where effect = alertErrorShow <$> runExceptT do
                   s <- foldM (saveEvent eventStore snapshotStore reducer) state events
                   pure (Internal (ResetState s))
        state = L.view _state model
internalUpdate {_state, reducer} model (ProcessEvents stateM events) = App.purely updatedModel
  where updatedModel = L.over _state (applyEvents reducer stateM events) model
internalUpdate eventApp model LoadEvents = {effects: App.batch [load], model}
  where load = loadEvents eventApp.eventStore eventApp.snapshotStore
internalUpdate {_state} model (ResetState s) =
  App.purely $ L.set _state s model
internalUpdate eventApp model (AlertError e) = {effects: App.batch [alertError e], model: model}
internalUpdate eventapp model DoNothing = App.purely model

externalUpdate :: forall st ev model msg.
                  App st ev model msg
               -> model
               -> msg
               -> App.Transition Aff model (MsgResult st ev msg)
externalUpdate {_state, reducer, eventStore, update} m msg = {effects: allEvents, model}
  where {effects, model, events} = update m msg
        allEvents = App.batch $ [pure (Internal (SaveEvents events))] <> mapmap External effects

mkUpdate :: forall st ev model msg.
            App st ev model msg
         -> model
         -> (MsgResult st ev msg)
         -> App.Transition Aff model (MsgResult st ev msg)
mkUpdate app model (External msg) = externalUpdate app model msg
mkUpdate app model (Internal msg) = internalUpdate app model msg

toApp :: forall st ev model msg. App st ev model msg -> App.App Aff Sub model (MsgResult st ev msg)
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
        -> Effect (App.AppInstance model (MsgResult st ev msg))
makeWithSelector eventApp selector =
  App.makeWithSelector interpreter app selector
  where interpreter = basicAff affErrorHandler `merge` (runTicker tickSecs)
        app = toApp eventApp
        tickSecs = snd <$> eventApp.tick


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
