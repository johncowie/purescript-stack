module Demo.AffApp where

import Prelude
import Data.DateTime.Instant (Instant)
import Data.Maybe (Maybe(..))

import Spork.Interpreter (Interpreter(..), basicEffect, basicAff, merge)
import Spork.EventQueue as EventQueue
import Spork.App as App
import Spork.Html as H
import Spork.Html.Events as E

import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Aff (Aff, makeAff)
import Effect.Now (now)
import Effect.Exception (Error)
import Effect.Console as C
import Effect.Exception.Unsafe (unsafeThrow)
import Effect.Timer as Timer
import Effect.Ref as Ref
import Data.Foldable (traverse_)
import Data.Either (Either(..))

type Model = {time :: Maybe String}
data Msg = DoNothing | Tick Instant | Log | TriggerLog

init :: App.Transition Aff Model Msg
init = App.purely {time: Nothing}

submitButton :: forall m. String -> m -> H.Html m
submitButton label msg = H.button [E.onClick (E.always_ msg)] [H.text label]

render :: Model -> H.Html Msg
render m = H.div [] [
    H.div [] [H.text $ show m.time]
  , H.div [] [submitButton "Click me!" TriggerLog]
  ]

delayedMessage :: Msg -> Aff Msg
delayedMessage msg = makeAff \handler -> do
  void $ Timer.setInterval 1000 (handler (Right msg))
  pure mempty

toAff :: forall a. Effect a -> Aff a
toAff eff = makeAff \handler -> do
  val <- eff
  handler (Right val)
  pure mempty

update :: Model -> Msg -> App.Transition Aff Model Msg
update model TriggerLog = {effects, model: model {time = Just "Loading.."}}
  where effects = App.lift $ delayedMessage Log
update model Log = {effects, model: model {time = Just "Loaded..."}}
  where effects = App.lift $ liftEffect $ do
                    n <- now
                    C.log $ show n
                    pure DoNothing
update model _ = App.purely model

subs :: Model -> App.Batch Sub Msg
subs model = App.lift (tickSub Tick)

app :: Instant -> App.App Aff Sub Model Msg
app i = { init
        , render
        , update
        , subs
        }

affErrorHandler :: Error -> Effect Unit
affErrorHandler err = C.log (show err)

-- EventQueueInstance Effect Msg -> Effect (Array Sub -> )


data Sub a = TickTime (Instant -> a)
derive instance functorSub :: Functor Sub

tickSub :: forall a. (Instant -> a) -> Sub a
tickSub = TickTime

runSubscriptions :: forall i. Interpreter Effect Sub i
runSubscriptions = Interpreter $ EventQueue.withAccumArray \queue -> do
    model <- Ref.new []

    let
        tick :: Effect Unit
        tick = do
            now <- now
            Ref.read model >>= traverse_ case _ of
                TickTime k -> queue.push (k now)
            queue.run

        commit :: Array (Sub i) -> Effect Unit
        commit new = do
            old <- Ref.read model
            Ref.write new model
            case old, new of
                [], _ -> void $ Timer.setInterval 5000 tick
                _, _  -> pure unit
            pure unit
    pure commit

main :: Effect Unit
main = do
  currentTime <- now
  inst <- App.makeWithSelector (basicAff affErrorHandler `merge` runSubscriptions) (app currentTime) "#app"
  inst.run
