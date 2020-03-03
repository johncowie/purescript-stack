module SporkDemo
(runApp)
where

import Prelude

import Effect (Effect)
-- import Effect.Console (log)
import Data.Const (Const)
import Data.Maybe (Maybe(..), maybe)
import Data.DateTime.Instant (Instant, unInstant) as Date
import Data.Time.Duration (Minutes(..), convertDuration) as Date
import Spork.App as App
import Spork.Html as Html
import Spork.Html (Html, styles, Style(..))
import Spork.Interpreter (merge, never)
import Utils.Spork.TimerSubscription (runSubscriptions, tickSub, Sub)

type Model = Maybe Date.Instant

data Msg = Tick Date.Instant

inMinutes :: Date.Instant -> Date.Minutes
inMinutes = Date.convertDuration <<< Date.unInstant

update :: Model -> Msg -> App.Transition (Const Void) Model Msg
update model (Tick time) = App.purely (Just time)

subs :: Model -> App.Batch Sub Msg
subs model = App.lift (tickSub Tick)

init :: App.Transition (Const Void) Model Msg
init = App.purely Nothing

render :: Model -> Html Msg
render model =
    Html.div [ styles [Style "width" "300px"] ]
        [ Html.text (show (maybe (Date.Minutes 0.0) inMinutes model)) ]

app :: App.App (Const Void) Sub Model Msg
app =
    { render
    , update
    , subs
    , init
    }

runApp :: Effect Unit
runApp = do
  inst <- App.makeWithSelector (never `merge` runSubscriptions) app "#app"
  inst.run
