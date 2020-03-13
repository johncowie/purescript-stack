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
import Spork.Html.Elements.Keyed as Keyed
import Spork.Html.Properties as P
import Spork.Html.Events as E
import Data.Maybe (Maybe(..), fromMaybe)
import Spork.Interpreter (merge, basicEffect)
import Utils.Spork.TimerSubscription (runSubscriptions, tickSub, Sub)
import Utils.DateTime (newDateTime)
import Utils.IdMap as IdMap
import Data.Tuple (Tuple(..))
import Data.Int (fromString, toNumber, floor)

data Msg = Tick Instant |
           UpdateAmount IdMap.Id String |
           LogAmount IdMap.Id (Maybe Instant) Int

type Model = {
  state :: GoalState,
  stats :: Stats,
  amountInputs :: IdMap.IdMap String
}

statsL :: L.Lens' Model Stats
statsL = L.lens _.stats (_ {stats = _})

stateL :: L.Lens' Model GoalState
stateL = L.lens _.state (_ {state = _})

amountInputsL :: L.Lens' Model (IdMap.IdMap String)
amountInputsL = L.lens _.amountInputs (_ {amountInputs = _})

testGoalState :: GoalState
testGoalState = processEvent (addGoalEvent "A Goal" (newDateTime 2020 1 1) (newDateTime 2020 4 1) 100)
                $ processEvent (addGoalEvent "Another silly Goal" (newDateTime 2020 1 2) (newDateTime 2020 5 1) 200)
                $ newGoalState

-- TODO load initial state from localstorage events (events and date passed in as arg)
-- TODO can do this without passing in as args, if an event is fired off to recalculate stats..
init :: Array Event -> Instant -> App.Transition Effect Model Msg
init _events dt = App.purely {state: state, stats: stats, amountInputs: IdMap.new}
  where state = testGoalState
        stats = calculateStats dt state

wrapWithClass :: forall a. String -> H.Html a -> H.Html a
wrapWithClass clazz node = H.div [P.classes [clazz]] [node]

repeatString :: Int -> String -> String
repeatString n s = case n of
                  x' | x' <= 0 -> ""
                  x' -> s <> repeatString (x' - 1) s

-- TODO implement
progressString :: Maybe GoalStats -> String
progressString statsM = (repeatString nX "X") <> repeatString nSpace " "
  where nX = progressScaled
        nSpace = progressLength - progressScaled
        progress = fromMaybe 0.0 $ _.progressPercentage <$> statsM
        progressScaled = floor $ progress * (toNumber progressLength / 100.0)
        progressLength = 20

amountDoneString :: Goal.Goal -> String
amountDoneString goal = show (L.view Goal.amountDoneL goal) <> "/" <> show (L.view Goal.targetL goal)

fromStringOrZero :: String -> Int
fromStringOrZero s = fromMaybe 0 (fromString s)

renderRow :: Model -> Tuple IdMap.Id Goal.Goal -> Tuple String (H.Html Msg)
renderRow model (Tuple id goal) =
  Tuple (show id) $ H.div [] [wrapWithClass "goalLabel" (H.text (L.view Goal.titleL goal)),
            wrapWithClass "amountDone" (H.text $ amountDoneString goal),
            wrapWithClass "progressBar" (H.text $ progressString statsM),
            wrapWithClass "amountInput" (H.input [P.placeholder "amount",
                                                  P.type_ P.InputText,
                                                  P.value inputStr,
                                                  E.onValueInput (E.always (UpdateAmount id))
                                                  ]),
            H.button
              [E.onClick (E.always_ (LogAmount id Nothing inputVal))]
              [H.text "Log"]
            ]
    where inputStr = fromMaybe "1" $ IdMap.get id $ L.view amountInputsL model
          inputVal = fromStringOrZero inputStr
          statsM = IdMap.get id $ L.view statsL model

render :: Model -> H.Html Msg
render model = Keyed.div [] $ map (renderRow model) $ IdMap.toList model.state

update :: Model → Msg → App.Transition Effect Model Msg
update model (Tick instant) =  App.purely $ L.set statsL (calculateStats instant model.state) model
update model (LogAmount id Nothing amount) = {effects, model}
  where effects = App.lift $ do
          nowInst <- now
          pure $ LogAmount id (Just nowInst) amount
update model (LogAmount id (Just now) amount) =
  App.purely $ L.set stateL updatedState $ L.set statsL (calculateStats now updatedState) model
  where updatedState = processEvent (addProgressEvent id now amount) model.state
update model (UpdateAmount id v) = App.purely $ L.over amountInputsL (IdMap.upsert id v) model

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
