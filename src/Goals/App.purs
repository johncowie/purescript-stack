module Goals.App where

import Prelude
import Effect (Effect)
import Utils.Lens as L
import Data.List (List(..), (:))
import Goals.Data.Goal as Goal
import Goals.Data.State (GoalState, newGoalState, processEvent, currentGoals, expiredGoals)
import Goals.Data.Stats (Stats, GoalStats, calculateStats)
import Goals.Data.Event (Event, addGoalEvent, addProgressEvent)
import Data.DateTime (DateTime)
import Data.DateTime.Instant (Instant, toDateTime)
import Effect.Now (now)
import Spork.App as App
import Spork.Html as H
import Spork.Html.Elements.Keyed as Keyed
import Spork.Html.Properties as P
import Spork.Html.Events as E
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Either (Either(..), either)
import Data.Foldable (foldr)
import Spork.Interpreter (merge, basicEffect)
import Utils.Spork.TimerSubscription (runSubscriptions, tickSub, Sub)
import Utils.DateTime (parseDate, showDate)
import Utils.IdMap as IdMap
import Data.Tuple (Tuple(..))
import Data.Int (fromString, toNumber, floor) as Int
import Data.Number (fromString) as Number
import Data.Map as M
import Effect.Exception.Unsafe (unsafeThrow)
import Data.Symbol (SProxy(..))
-- Webstorage stuff
import Utils.LocalJsonStorage (load, store) as JsonStorage

data Msg = Tick Instant |
           UpdateStringInput (L.Lens' Model String) String |
           LogAmount IdMap.Id (Maybe Instant) |
           AddGoal (Maybe Instant) |
           DoNothing

type GoalForm = {
  goalName :: String,
  target :: String,
  startDate :: String,
  endDate :: String
}

type Model = {
  lastUpdate :: Maybe Instant,
  state :: GoalState,
  stats :: Stats,
  amountInputs :: IdMap.IdMap String,
  goalForm :: GoalForm,
  inputs ::  M.Map String String
  -- inputs :: M.Map String StringInputState
}

-- could use the Data.Default abstraction
emptyModel :: Model
emptyModel = {
  lastUpdate: Nothing,
  state: newGoalState,
  stats: IdMap.new,
  amountInputs: IdMap.new,
  goalForm: {
    goalName: "",
    target: "",
    startDate: "",
    endDate: ""
  },
  inputs: M.empty
}


type StringInputState = {
  val :: String,
  error :: Maybe String
}

-- todo abstract out model
type StringInput a = {
  validator :: String -> Either String a,
  inputLabel :: String,
  inputId :: String,
  lens :: L.Lens' Model String,
  placeholder :: String
}

parseInt :: String -> Either String Int
parseInt s = case Int.fromString s of
  Nothing -> Left "Not a valid integer"
  (Just i) -> Right i

parseNumber :: String -> Either String Number
parseNumber s = case Number.fromString s of
  Nothing -> Left "Not a valid number"
  (Just i) -> Right i

nonEmptyString :: String -> Either String String
nonEmptyString "" = Left "Can't be empty"
nonEmptyString s = Right s

-- abstract out message
stringInput :: forall a. String -> (String -> Either String a) -> String -> StringInput a
stringInput placeholder validator inputId = {
  validator: validator,
  inputLabel: "unused",
  lens: (inputsL >>> mapValL "" inputId),
  placeholder: placeholder,
  inputId: inputId -- TODO use this as basis of lens
}

goalNameInput :: StringInput String
goalNameInput = stringInput "goal name" nonEmptyString "goalName"

goalTargetInput :: StringInput Int
goalTargetInput = stringInput "target" parseInt "goalTarget"

goalStartInput :: StringInput DateTime
goalStartInput = stringInput "start date" parseDate "goalStartDate"

goalEndInput :: StringInput DateTime
goalEndInput = stringInput "end date" parseDate "goalEndDate"

amountInput :: IdMap.Id -> StringInput Number
amountInput id = stringInput "amount" parseNumber ("amount-" <> show id)

-- TODO abstract out msg/model and move to utils
renderStringInput :: forall a. StringInput a -> Model -> H.Html Msg
renderStringInput input model =
  H.div
  []
  [H.input [P.placeholder input.placeholder,
            P.type_ P.InputText,
            P.value (L.view lens model),
            E.onValueInput (E.always (UpdateStringInput lens))]]
  where lens = (input.lens :: L.Lens' Model String)

parseStringInputUnsafe :: forall a. StringInput a -> Model -> a
parseStringInputUnsafe input model = either unsafeThrow identity $ input.validator $ L.view input.lens model

--- composing these inputs together could construct lens for SubmitForm action to take parsed vals from model

_lastUpdate :: L.Lens' Model (Maybe Instant)
_lastUpdate = L.prop (SProxy :: SProxy "lastUpdate")

statsL :: L.Lens' Model Stats
statsL = L.prop (SProxy :: SProxy "stats")

stateL :: L.Lens' Model GoalState
stateL = L.prop (SProxy :: SProxy "state")

inputsL :: L.Lens' Model (M.Map String String)
inputsL = L.prop (SProxy :: SProxy "inputs")

mapValL :: forall k v. (Ord k) => v -> k -> L.Lens' (M.Map k v) v
mapValL default id = L.lens get set
  where get m = fromMaybe default $ M.lookup id m
        set m v = M.insert id v m

amountInputsL :: L.Lens' Model (IdMap.IdMap String)
amountInputsL = L.prop (SProxy :: SProxy "amountInputs")

idMapValL :: forall a. a -> IdMap.Id -> L.Lens' (IdMap.IdMap a) a
idMapValL = mapValL

modelAmountInputL :: IdMap.Id -> L.Lens' Model String
modelAmountInputL id = amountInputsL >>> (idMapValL "" id)

goalFormL :: L.Lens' Model GoalForm
goalFormL = L.prop (SProxy :: SProxy "goalForm")

goalNameL :: L.Lens' GoalForm String
goalNameL = L.prop (SProxy :: SProxy "goalName")

goalTargetL :: L.Lens' GoalForm String
goalTargetL = L.prop (SProxy :: SProxy "target")

goalStartDateL :: L.Lens' GoalForm String
goalStartDateL = L.prop (SProxy :: SProxy "startDate")

goalEndDateL :: L.Lens' GoalForm String
goalEndDateL = L.prop (SProxy :: SProxy "endDate")

-- TODO load initial state from localstorage events (events and date passed in as arg)
-- TODO can do this without passing in as args, if an event is fired off to recalculate stats..
init :: List Event -> Instant -> App.Transition Effect Model Msg
init events dt = App.purely (emptyModel {lastUpdate = Just dt, state = state, stats = stats})
  where state = foldr processEvent newGoalState events
        stats = calculateStats dt state

wrapWithClass :: forall a. String -> H.Html a -> H.Html a
wrapWithClass clazz node = H.div [P.classes [clazz]] [node]

repeatString :: Int -> String -> String
repeatString n s = case n of
                  x' | x' <= 0 -> ""
                  x' -> s <> repeatString (x' - 1) s

scalePercentage :: Int -> Number -> Int
scalePercentage points percentage = Int.floor $ percentage * (Int.toNumber points / 100.0)

isOnTrack :: GoalStats -> Boolean
isOnTrack  = (>=) 0  <<< _.onTrackRequired

progressBar :: forall a. Maybe GoalStats -> H.Html a
progressBar statsM = H.div [P.classes ["progress-bar"]]
                           [H.span [P.classes [statusClass]] [H.text (repeatString nX "X")],
                            H.span [P.classes ["progress-grey"]] [H.text (repeatString nXRem "X")],
                            H.span [] [(H.text (repeatString nSpace " "))]]
  where nX = fromMaybe 0 $ scalePercentage charLength <$> _.progressPercentage <$> statsM
        nXRequired = fromMaybe 0 $ scalePercentage charLength <$> _.timeElapsedPercentage <$> statsM
        nXRem = max (nXRequired - nX) 0
        nSpace = charLength - (nX + nXRem)
        charLength = 50
        statusClass = case isOnTrack <$> statsM of
          Nothing -> ""
          Just false -> "progress-red"
          Just true -> "progress-green"

renderOnTrackRequired :: forall a. Maybe GoalStats -> H.Html a
renderOnTrackRequired = H.text <<< show <<< fromMaybe 0 <<< map _.onTrackRequired

amountDoneString :: Goal.Goal -> String
amountDoneString goal = show (L.view Goal._amountDone goal) <> "/" <> show (L.view Goal._target goal)

fromStringOrZero :: String -> Int
fromStringOrZero s = fromMaybe 0 (Int.fromString s)

submitButton :: forall m. String -> m -> H.Html m
submitButton label msg = H.button [E.onClick (E.always_ msg)] [H.text label]

-- TODO move towards each component using a lens
renderLiveGoal :: Model -> Tuple IdMap.Id Goal.Goal -> Tuple String (H.Html Msg)
renderLiveGoal model (Tuple id goal) =
  Tuple (show id) $ H.div [] [wrapWithClass "goalLabel" $ H.text (show id <> ": " <> L.view Goal._title goal),
                              wrapWithClass "amountDone" $ (H.text $ amountDoneString goal),
                              wrapWithClass "onTrackRequired" $ renderOnTrackRequired statsM,
                              progressBar statsM,
                              wrapWithClass "amountInput" $ renderStringInput (amountInput id) model,
                              submitButton "Log" (LogAmount id Nothing)
                              ]
    where statsM = IdMap.get id $ L.view statsL model

renderExpiredGoal :: Model -> Tuple IdMap.Id Goal.Goal -> Tuple String (H.Html Msg)
renderExpiredGoal model (Tuple id goal) =
  Tuple (show id) $ H.div [] [H.text $ L.view Goal._title goal,
                              H.text $ " - ",
                              H.text $ showDate $ L.view Goal._start goal,
                              H.text $ " - ",
                              H.text $ showDate $ L.view Goal._end goal
                              ]

renderCurrentGoalList :: Model -> H.Html Msg
renderCurrentGoalList model = Keyed.div [] $ map (renderLiveGoal model) $
  case model.lastUpdate of
    Nothing -> []
    (Just now) -> IdMap.toArray $ currentGoals (toDateTime now) $ model.state

renderExpiredGoalList :: Model -> H.Html Msg
renderExpiredGoalList model = Keyed.div [] $ map (renderExpiredGoal model) $
  case model.lastUpdate of
    Nothing -> []
    (Just now) -> IdMap.toArray $ expiredGoals (toDateTime now) $ model.state

renderGoalForm :: Model -> H.Html Msg
renderGoalForm m = H.div [] [
  H.h3 [] [H.text "Add Goal"],
  inputRow "Goal name: " $ renderStringInput goalNameInput m,
  inputRow "Target: " $ renderStringInput goalTargetInput m,
  inputRow "Start Date: " $ renderStringInput goalStartInput m,
  inputRow "End Date: " $ renderStringInput goalEndInput m,
  submitButton "Add Goal" (AddGoal Nothing)
] where inputRow label input = H.div [] [H.label [] [(H.text label)], input]

render :: Model -> H.Html Msg
render model = H.div [] [H.h3 [] [H.text "Current goals"],
                         renderCurrentGoalList model,
                         H.h3 [] [H.text "Expired goals"],
                         renderExpiredGoalList model,
                         renderGoalForm model]

storageKey :: String
storageKey = "goals"

loadEvents :: Effect (List Event)
loadEvents = do
  eventsE <- JsonStorage.load storageKey
  let eventsM = either unsafeThrow identity eventsE
  pure $ fromMaybe Nil eventsM

storeEvents :: List Event -> Effect Unit
storeEvents = JsonStorage.store storageKey

storeEvent :: Event -> Effect Unit
storeEvent event = do
  events <- loadEvents
  storeEvents $ event : events

fireStateEvent :: Instant -> Model -> Event -> App.Transition Effect Model Msg
fireStateEvent now model event = {effects, model: updatedModel}
  where effects = App.lift $ do
          storeEvent event
          pure $ DoNothing
        updatedModel = L.set stateL updatedState $ L.set statsL (calculateStats now updatedState) model
        updatedState = processEvent event model.state
-- store event in local storage, fire event to update model and stats

update :: Model → Msg → App.Transition Effect Model Msg
update model (Tick instant) =  App.purely $
  L.set statsL (calculateStats instant model.state) $
  L.set _lastUpdate (Just instant) $
  model
update model (LogAmount id Nothing) = {effects, model}
  where effects = App.lift $ do
          nowInst <- now
          pure $ LogAmount id (Just nowInst)
update model (AddGoal Nothing) = {effects, model}
  where effects = App.lift $ do
          nowInst <- now
          pure $ AddGoal (Just nowInst)
update model (LogAmount id (Just now)) = fireStateEvent now clearedInputs (addProgressEvent id now amount)
  where amount = parseStringInputUnsafe (amountInput id) model
        clearedInputs = L.set (amountInput id).lens "" model
update model (AddGoal (Just now)) = fireStateEvent now clearedInputs (addGoalEvent title startDate endDate target)
  where title = parseStringInputUnsafe goalNameInput model
        startDate = parseStringInputUnsafe goalStartInput model
        endDate = parseStringInputUnsafe goalEndInput model
        target = parseStringInputUnsafe goalTargetInput model
        clearedInputs = L.set goalNameInput.lens "" $
                        L.set goalStartInput.lens "" $
                        L.set goalEndInput.lens "" $
                        L.set goalTargetInput.lens "" $ model
update model (DoNothing) = App.purely model
update model (UpdateStringInput stringL input) = App.purely $ L.set stringL input model

subs :: Model -> App.Batch Sub Msg
subs model = App.lift (tickSub Tick)

app :: List Event -> Instant -> App.App Effect Sub Model Msg
app events now = {
    render
  , update
  , subs: subs
  , init: init events now
}

runApp :: Effect Unit
runApp = do
  events <- loadEvents
  currentTime <- now
  inst <- App.makeWithSelector (basicEffect `merge` runSubscriptions) (app events currentTime) "#app"
  inst.run
