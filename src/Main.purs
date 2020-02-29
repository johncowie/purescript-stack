module Main where

import Prelude

import Effect (Effect)
import Effect.Ref as Ref
-- import Effect.Console (log)
import Effect.Now (now) as Date
import Effect.Timer as Timer
import Data.Foldable (traverse_)
import Data.Const (Const)
import Data.Maybe (Maybe(..), maybe)
import Data.DateTime.Instant (Instant, unInstant) as Date
import Data.Time.Duration (Minutes(..), convertDuration) as Date
import Math as Math
import Spork.App as App
import Spork.EventQueue as EventQueue
import Spork.Html as Html
import Spork.Html (Html, styles, Style(..))
import Spork.Interpreter (Interpreter(..), merge, never)
import Common.Svg (svg, viewBox, circle, cx, cy, r, fill, line, x1, x2, y1, y2, stroke)

type Model =
    Maybe Date.Instant

data Sub a =
    TickTime (Date.Instant -> a)

derive instance functorSub :: Functor Sub

data Msg
    = Tick Date.Instant

inMinutes :: Date.Instant -> Date.Minutes
inMinutes =
    Date.convertDuration <<< Date.unInstant

turns :: Number -> Number
turns ts =
    2.0 * Math.pi * ts

update :: Model -> Msg -> App.Transition (Const Void) Model Msg
update model msg =
    case msg of
        Tick time ->
            App.purely (Just time)

subs :: Model -> App.Batch Sub Msg
subs model =
    App.lift (TickTime Tick)

init :: App.Transition (Const Void) Model Msg
init =
    App.purely Nothing

render :: Model -> Html Msg
render model =
    let
        Date.Minutes minutes =
            maybe (Date.Minutes 0.0) inMinutes model

        angle =
            turns minutes

        handX =
            show (50.0 + 40.0 * Math.cos angle)

        handY =
            show (50.0 + 40.0 * Math.sin angle)
    in
    Html.div [ styles [Style "width" "300px"] ]
        [ svg [ viewBox "0 0 100 100"]
            [ circle [ cx "50", cy "50", r "45", fill "#0B79CE" ] []
            , line [ x1 "50", y1 "50", x2 handX, y2 handY, stroke "#023963" ] []
            ]
        ]

runSubscriptions :: forall i. Interpreter Effect Sub i
runSubscriptions = Interpreter $ EventQueue.withAccumArray \queue -> do
    model <- Ref.new []

    let
        tick :: Effect Unit
        tick = do
            now <- Date.now
            Ref.read model >>= traverse_ case _ of
                TickTime k -> queue.push (k now)
            queue.run

        commit :: Array (Sub i) -> Effect Unit
        commit new = do
            old <- Ref.read model
            Ref.write new model
            case old, new of
                [], _ -> void $ Timer.setInterval 1000 tick
                _, _  -> pure unit

            pure unit

    pure commit

app :: App.App (Const Void) Sub Model Msg
app =
    { render
    , update
    , subs
    , init
    }

main :: Effect Unit
main = do
  inst <- App.makeWithSelector (never `merge` runSubscriptions) app "#app"
  inst.run
