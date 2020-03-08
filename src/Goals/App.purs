module Goals.App where

import Prelude
import Effect (Effect)
import Data.Lens as L
import Goals.Data.Goal as Goal
import Goals.Data.State (GoalState, newGoalState, processEvent)
import Goals.Data.Stats (Stats, GoalStats, calculateStats)
import Goals.Data.Event (Event, addGoalEvent, addProgressEvent)
import Data.DateTime.Instant (Instant)
import Effect.Now (now)
import Spork.App as App
import Spork.Html as H
import Spork.Html.Properties as P
import Spork.Html.Events as E
import Data.Maybe (Maybe(..))
import Spork.Interpreter (merge, basicEffect)
import Utils.Spork.TimerSubscription (runSubscriptions, tickSub, Sub)
import Utils.DateTime (newDateTime)
import Utils.IdMap as IdMap
import Data.Tuple (Tuple(..))

data Msg = Tick Instant | LogAmount IdMap.Id (Maybe Instant) Int

type Model = {
  state :: GoalState,
  stats :: Stats
}

statsL :: L.Lens' Model Stats
statsL = L.lens _.stats (_ {stats = _})

stateL :: L.Lens' Model GoalState
stateL = L.lens _.state (_ {state = _})

testGoalState :: GoalState
testGoalState = processEvent (addGoalEvent "A Goal" (newDateTime 2020 1 1) (newDateTime 2020 4 1) 100)
                $ processEvent (addGoalEvent "Another silly Goal" (newDateTime 2020 1 2) (newDateTime 2020 5 1) 200)
                $ newGoalState

-- TODO load initial state from localstorage events (events and date passed in as arg)
-- TODO can do this without passing in as args, if an event is fired off to recalculate stats..
init :: Array Event -> Instant -> App.Transition Effect Model Msg
init _events dt = App.purely {state: state, stats: stats}
  where state = testGoalState
        stats = calculateStats dt state

wrapWithClass :: forall a. String -> H.Html a -> H.Html a
wrapWithClass clazz node = H.div [P.classes [clazz]] [node]

-- TODO implement
progressString :: Maybe GoalStats -> String
progressString _ = "XXXX     "

amountDoneString :: Goal.Goal -> String
amountDoneString goal = show (L.view Goal.amountDoneL goal) <> "/" <> show (L.view Goal.targetL goal)


renderRow :: Tuple IdMap.Id (Tuple Goal.Goal (Maybe GoalStats)) -> H.Html Msg
renderRow (Tuple id (Tuple goal statsM)) =
  H.div [] [wrapWithClass "goalLabel" (H.text (L.view Goal.titleL goal)),
            wrapWithClass "amountDone" (H.text $ amountDoneString goal),
            wrapWithClass "progressBar" (H.text $ progressString statsM),
            wrapWithClass "amountInput" (H.input [P.placeholder "amount", P.value ""]),
            H.button [E.onClick (E.always_ (LogAmount id Nothing 2))] [(H.text "Log")]]

addGoalStats :: GoalState -> Stats -> IdMap.IdMap (Tuple Goal.Goal (Maybe GoalStats))
addGoalStats = IdMap.rightJoinMap

render :: Model -> H.Html Msg
render model = H.div [] $ map renderRow $ IdMap.toList $ addGoalStats model.state model.stats

update :: Model → Msg → App.Transition Effect Model Msg
update model (Tick instant) =  App.purely $ L.set statsL (calculateStats instant model.state) model
update model (LogAmount id Nothing amount) = {effects, model}
  where effects = App.lift $ do
          nowInst <- now
          pure $ LogAmount id (Just nowInst) amount
update model (LogAmount id (Just now) amount) = App.purely $ L.over stateL (processEvent (addProgressEvent id now amount)) model

subs :: Model -> App.Batch Sub Msg
subs model = App.lift (tickSub Tick)

app :: Instant -> App.App Effect Sub Model Msg
app now = {
    render
  , update
  , subs: subs
  , init: init [] now
}

runApp :: Effect Unit
runApp = do
  currentTime <- now
  inst <- App.makeWithSelector (basicEffect `merge` runSubscriptions) (app currentTime) "#app"
  inst.run

-- TODO
-- all apps ultimately need
--  function for loading state from State events
--  rendering of state
--  update function
