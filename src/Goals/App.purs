module Goals.App where

import Prelude
import Effect (Effect)
import Data.Lens as L
import Goals.Data.Goal as Goal
import Goals.Data.State (GoalState, newGoalState, processEvent)
import Goals.Data.Stats (Stats, calculateStats)
import Goals.Data.Event (Event, addGoalEvent)
import Data.DateTime.Instant (Instant, toDateTime)
import Effect.Now (now)
import Spork.App as App
import Spork.Html as H
import Data.Const (Const)
import Spork.Interpreter (never, merge)
import Utils.Spork.TimerSubscription (runSubscriptions, tickSub, Sub)
import Utils.DateTime (newDateTime)
import Utils.IdMap as IdMap
import Data.Tuple (Tuple(..))

data Msg = Tick Instant

type Model = {
  state :: GoalState,
  stats :: Stats
}

testGoalState :: GoalState
testGoalState = processEvent (addGoalEvent "A Goal" (newDateTime 2020 3 1) (newDateTime 2020 4 1) 100)
                $ processEvent (addGoalEvent "Dick Goal" (newDateTime 2020 4 2) (newDateTime 2020 5 1) 200)
                $ newGoalState

-- TODO load initial state from localstorage events (events and date passed in as arg)
init :: Array Event -> Instant -> App.Transition (Const Void) Model Msg
init _events dt = App.purely {state: state, stats: stats}
  where state = testGoalState
        stats = calculateStats (toDateTime dt) state

-- TODO render goal row

renderRow :: Tuple IdMap.Id Goal.Goal -> H.Html Msg
renderRow (Tuple id goal) = H.div [] [H.text (L.view Goal.titleL goal)]

render :: Model -> H.Html Msg
render model = H.div [] $ map renderRow $ IdMap.toList model.state

update :: Model → Msg → App.Transition (Const Void) Model Msg
update model _action = App.purely model

subs :: Model -> App.Batch Sub Msg
subs model = App.lift (tickSub Tick)

app :: Instant -> App.App (Const Void) Sub Model Msg
app now = {
    render
  , update
  , subs: subs
  , init: init [] now
}

runApp :: Effect Unit
runApp = do
  currentTime <- now
  inst <- App.makeWithSelector (never `merge` runSubscriptions) (app currentTime) "#app"
  inst.run

-- TODO
-- all apps ultimately need
--  function for loading state from State events
--  rendering of state
--  update function
