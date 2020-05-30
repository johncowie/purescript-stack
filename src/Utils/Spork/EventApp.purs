module Utils.Spork.EventApp where

import Prelude

import Data.Time.Duration (Seconds)
import Data.Maybe (Maybe, maybe)
import Data.Tuple (Tuple, fst, snd)
import Data.DateTime.Instant (Instant)

import Effect (Effect)
import Effect.Aff (Aff)

import Spork.App as App
import Spork.Interpreter (basicAff, merge)
import Spork.Html as H


import Utils.Spork.TimerSubscription (runTicker, Sub, tickSub)
import Utils.Alert (alert)

type Transition model msg = {
  effects :: Array (Aff msg)
, model :: model
}

type App model msg = {
  render :: model -> H.Html msg
, update :: model -> msg -> Transition model msg
, init :: Transition model msg
, tick :: Maybe (Tuple (Instant -> msg) Seconds)
}

purely :: forall model msg. model -> Transition model msg
purely model = {effects: [], model}

toApp :: forall model msg. App model msg -> App.App Aff Sub model msg
toApp eventApp = {render, update, subs, init}
  where update model msg = {effects: App.batch effects, model}
          where {effects, model} = eventApp.update model msg
        render = eventApp.render
        subs m = maybe (App.batch []) App.lift tickSubM
          where tickSubM = tickSub <$> fst <$> eventApp.tick
        init = {effects: App.batch effects, model}
          where {effects, model} = eventApp.init

affErrorHandler :: forall err. (Show err) => err ->  Effect Unit
affErrorHandler err = alert (show err)

makeWithSelector :: forall model msg.
           App model msg
        -> String
        -> Effect (App.AppInstance model msg)
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
