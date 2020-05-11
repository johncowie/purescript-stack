module Goals.App where

import Prelude
import Effect (Effect)
import Utils.Lens as L
import Data.List (List(..), (:))
import Data.List as List
import Goals.Data.Goal as Goal
import Goals.Data.Goal (Goal)
import Goals.Data.State as St
import Goals.Data.Event (Event, addGoalEvent, addProgressEvent, restartGoalEvent, undoEvent)
import Data.Date (Date)
import Data.DateTime (date)
import Data.DateTime.Instant (Instant)
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
import Utils.DateTime (showDate, showDayMonth, dateToDateTime)
import Utils.IdMap as IdMap
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested (tuple3)
import Data.Int (fromString, toNumber, floor) as Int
import Data.Number (fromString) as Number
import Data.Map as M
import Effect.Exception.Unsafe (unsafeThrow)
import Data.Symbol (SProxy(..))
-- Webstorage stuff
import Utils.LocalJsonStorage (load, store) as JsonStorage
import Utils.NumberFormat (toDP)
import Utils.Url as Url
import Utils.Fixed as DF
import Effect.Console as Console
import Utils.Components.Input as Input
import Utils.Components.Input (Inputs, StringInput)
import Data.Array as Array

data Msg = Tick Instant |
           UpdateStringInput (L.Lens' Model String) String |
           LogAmount IdMap.Id (Maybe Instant) |
           AddGoal |
           RestartGoal IdMap.Id |
           StoreEvent Event |
           UndoEvent Int |
           DoNothing

data Page = GoalPage | EventsPage

type GoalForm = {
  goalName :: String,
  target :: String,
  startDate :: String,
  endDate :: String
}

type Model = {
  page :: Page,
  lastUpdate :: Instant,
  state :: St.GoalState,
  events :: List Event,
  amountInputs :: IdMap.IdMap String,
  goalForm :: GoalForm,
  inputs ::  Input.Inputs
  -- inputs :: M.Map String StringInputState
}

-- could use the Data.Default abstraction
emptyModel :: Instant -> Model
emptyModel now = {
  page: GoalPage,
  lastUpdate: now,
  state: St.newGoalState,
  events: Nil,
  amountInputs: IdMap.new,
  goalForm: {
    goalName: "",
    target: "",
    startDate: "",
    endDate: ""
  },
  inputs: M.empty
}

parseInt :: String -> Either String Int
parseInt s = case Int.fromString s of
  Nothing -> Left "Not a valid integer"
  (Just i) -> Right i

parseNumber :: String -> Either String Number
parseNumber s = case Number.fromString s of
  Nothing -> Left "Not a valid number"
  (Just i) -> Right i

anyString :: String -> Either String String
anyString s = Right s

goalNameInput :: StringInput Model String
goalNameInput = Input.stringInput _inputs "goalName"

goalTargetInput :: StringInput Model Int
goalTargetInput = Input.stringInput _inputs "goalTarget"

goalStartInput :: StringInput Model Date
goalStartInput = Input.stringInput _inputs "goalStartDate"

goalEndInput :: StringInput Model Date
goalEndInput = Input.stringInput _inputs "goalEndDate"

amountInput :: IdMap.Id -> StringInput Model Number
amountInput id = Input.stringInput _inputs ("amount-" <> show id)

commentInput :: IdMap.Id -> StringInput Model (Maybe String)
commentInput id = Input.stringInput _inputs ("comment-" <> show id)

restartGoalNameInput :: IdMap.Id -> StringInput Model String
restartGoalNameInput id = Input.stringInput _inputs ("restartGoalTitle" <> show id)

restartGoalStartInput :: IdMap.Id -> StringInput Model Date
restartGoalStartInput id = Input.stringInput _inputs ("restartGoalStartDate" <> show id)

restartGoalEndInput :: IdMap.Id -> StringInput Model Date
restartGoalEndInput id = Input.stringInput _inputs ("restartGoalEndDate" <> show id)

restartGoalTargetInput :: IdMap.Id -> StringInput Model Int
restartGoalTargetInput id = Input.stringInput _inputs ("restartGoalTarget" <> show id)

--- composing these inputs together could construct lens for SubmitForm action to take parsed vals from model

_lastUpdate :: L.Lens' Model Instant
_lastUpdate = L.prop (SProxy :: SProxy "lastUpdate")

_page :: L.Lens' Model Page
_page = L.prop (SProxy :: SProxy "page")

stateL :: L.Lens' Model St.GoalState
stateL = L.prop (SProxy :: SProxy "state")

_events :: L.Lens' Model (List Event)
_events = L.prop (SProxy :: SProxy "events")

_inputs :: L.Lens' Model Inputs
_inputs = L.prop (SProxy :: SProxy "inputs")

mapValL :: forall k v. (Ord k) => v -> k -> L.Lens' (M.Map k v) v
mapValL default id = L.lens get set
  where get m = fromMaybe default $ M.lookup id m
        set m v = M.insert id v m

init :: Page -> List Event -> Instant -> App.Transition Effect Model Msg
init page events dt = App.purely ((emptyModel dt) {page = page, state = state, events = events})
  where state = foldr St.processEvent St.newGoalState events

wrapWithClass :: forall a. String -> H.Html a -> H.Html a
wrapWithClass clazz node = H.div [P.classes [clazz]] [node]

repeatString :: Int -> String -> String
repeatString n s = case n of
                  x' | x' <= 0 -> ""
                  x' -> s <> repeatString (x' - 1) s

scalePercentage :: Int -> Number -> Int
scalePercentage points percentage = Int.floor $ percentage * (Int.toNumber points / 100.0)

isOnTrack :: Instant -> Goal -> Boolean
isOnTrack now goal = (>=) 0.0 $ Goal.onTrackRequired now goal

progressBar :: forall a. Instant -> Goal -> H.Html a
progressBar now goal = H.div [P.classes ["progress-bar"]]
                             [H.span [P.classes [statusClass]] [H.text (repeatString nX "X")],
                              H.span [P.classes ["progress-grey"]] [H.text (repeatString nXRem "X")],
                              H.span [] [(H.text (repeatString nSpace " "))]]
  where nX = scalePercentage charLength $ Goal.progressPercentage goal
        nXRequired = scalePercentage charLength $ Goal.timeElapsedPercentage now goal
        nXRem = max (nXRequired - nX) 0
        nSpace = charLength - (nX + nXRem)
        charLength = 30
        statusClass = if isOnTrack now goal then "progress-green" else "progress-red"

renderOnTrackRequired :: forall a. Instant -> Goal -> H.Html a
renderOnTrackRequired now goal = H.text $ toDP 0 $ Goal.onTrackRequired now goal

amountDoneString :: Goal.Goal -> String
amountDoneString goal = toDP 1 (L.view Goal._amountDone goal) <> "/" <> show (L.view Goal._target goal)

fromStringOrZero :: String -> Int
fromStringOrZero s = fromMaybe 0 (Int.fromString s)

submitButton :: forall m. String -> m -> H.Html m
submitButton label msg = H.button [E.onClick (E.always_ msg)] [H.text label]

-- TODO move towards each component using a lens
renderLiveGoal :: Model -> Tuple IdMap.Id Goal.Goal -> Tuple String (H.Html Msg)
renderLiveGoal model (Tuple id goal) =
  Tuple (show id) $ H.div [] [wrapWithClass "goal-label" $ H.text (show id <> ": " <> L.view Goal._title goal),
                              wrapWithClass "goal-end" $ H.text $ showDate $ date $ L.view Goal._end goal,
                              wrapWithClass "amount-done" $ (H.text $ amountDoneString goal),
                              wrapWithClass "on-track-required" $ renderOnTrackRequired model.lastUpdate goal,
                              progressBar model.lastUpdate goal,
                              wrapWithClass "amount-input" $ Input.renderStringInput UpdateStringInput (amountInput id) "amount" model,
                              wrapWithClass "amount-input" $ Input.renderStringInput UpdateStringInput (commentInput id) "comment" model,
                              submitButton "Log" (LogAmount id Nothing)
                              ]

renderRestartGoalForm :: IdMap.Id -> Model -> H.Html Msg
renderRestartGoalForm id model =
  H.div [P.classes ["expired-goal-form"]] $
  [ Input.renderStringInput UpdateStringInput (restartGoalNameInput id) "goal name" model
  , Input.renderStringInput UpdateStringInput (restartGoalStartInput id) "start date" model
  , Input.renderStringInput UpdateStringInput (restartGoalEndInput id) "end date" model
  , Input.renderStringInput UpdateStringInput (restartGoalTargetInput id) "target" model
  , submitButton "Restart Goal" (RestartGoal id)
  ]

renderExpiredGoal :: Model -> Tuple IdMap.Id Goal.Goal -> Tuple String (H.Html Msg)
renderExpiredGoal model (Tuple id goal) =
  Tuple (show id) $ H.div [P.classes ["expired-goal"]] $ [
    wrapWithClass "goal-label" $ H.text $ L.view Goal._title goal,
    H.text $ showDayMonth $ L.view Goal._start goal,
    H.text $ " - ",
    H.text $ showDayMonth $ L.view Goal._end goal,
    H.text  " - ",
    H.text $ show (L.view Goal._amountDone goal) <> "/" <> show (L.view Goal._target goal)
    ]
    <>
    if needsRestarting
    then [renderRestartGoalForm id model]
    else []
  where needsRestarting = not $ St.hasSuccessor id model.state

renderFutureGoal :: Model -> Tuple IdMap.Id Goal.Goal -> Tuple String (H.Html Msg)
renderFutureGoal model (Tuple id goal) =
  Tuple (show id) $ H.div [P.classes ["future-goal"]] $ [
    H.text $ L.view Goal._title goal,
    H.text $ " - ",
    H.text $ showDate $ date $ L.view Goal._start goal,
    H.text $ " - ",
    H.text $ showDate $ date $ L.view Goal._end goal,
    H.text $ " - ",
    H.text $ show $ L.view Goal._target goal
  ]

renderCurrentGoalList :: Model -> H.Html Msg
renderCurrentGoalList model = Keyed.div [] $ map (renderLiveGoal model) $
    Array.sortWith sortF $ IdMap.toArray $ M.filter (Goal.isInProgress now) $ model.state
    where sortF (Tuple id goal) = tuple3 (toFixed $ -1.0 * (Goal.requiredPercentage now goal)) (L.view Goal._end goal) (L.view Goal._title goal)
          now = model.lastUpdate
          toFixed n = case DF.fromNumber n of
                        Nothing -> unsafeThrow "ARGHH"
                        (Just f) -> (f :: DF.Fixed DF.P10000)

renderExpiredGoalList :: Model -> H.Html Msg
renderExpiredGoalList model = Keyed.div [] $ map (renderExpiredGoal model) $
  Array.filter noSuccessor $ IdMap.toArray $ M.filter (Goal.isExpired model.lastUpdate) model.state
    where noSuccessor (Tuple id goal) = not $ St.hasSuccessor id (L.view stateL model)

renderFutureGoalList :: Model -> H.Html Msg
renderFutureGoalList model = Keyed.div [] $ map (renderFutureGoal model) $
  IdMap.toArray $ M.filter (Goal.isFuture model.lastUpdate) $ model.state

renderGoalForm :: Model -> H.Html Msg
renderGoalForm m = H.div [] [
  H.h3 [] [H.text "Add Goal"],
  inputRow "Goal name: " $ Input.renderStringInput UpdateStringInput goalNameInput "goal name" m,
  inputRow "Target: " $ Input.renderStringInput UpdateStringInput goalTargetInput "target" m,
  inputRow "Start Date: " $ Input.renderStringInput UpdateStringInput goalStartInput "start date" m,
  inputRow "End Date: " $ Input.renderStringInput UpdateStringInput goalEndInput "end date" m,
  submitButton "Add Goal" AddGoal
] where inputRow label input = H.div [] [H.label [] [(H.text label)], input]

renderGoalsPage :: Model -> H.Html Msg
renderGoalsPage model = H.div [] [H.h3 [] [H.text "Current goals"],
                                  renderCurrentGoalList model,
                                  H.h3 [] [H.text "Expired goals"],
                                  renderExpiredGoalList model,
                                  H.h3 [] [H.text "Future goals"],
                                  renderFutureGoalList model,
                                  renderGoalForm model]

renderEvent :: Tuple Int Event -> H.Html Msg
renderEvent (Tuple index event) =
  H.div [] [ H.text (show index <> ": " <> show event)
           , submitButton "Undo" (UndoEvent index)
           ]
-- TODO Also need a button for triggering a removal.

renderEventListPage :: Model -> H.Html Msg
renderEventListPage model =
  H.div [] [
    H.h3 [] [H.text "Events"],
    H.div [] $ map renderEvent $ Array.take 20 $ addIndices $ Array.fromFoldable model.events
  ]
  where addIndices l = Array.zip (Array.range 0 (Array.length l - 1)) l

renderPage :: Page -> Model -> H.Html Msg
renderPage GoalPage = renderGoalsPage
renderPage EventsPage = renderEventListPage

render :: Model -> H.Html Msg
render model = renderPage (L.view _page model) model

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

deleteEvent :: Int -> Effect Unit
deleteEvent idx = do
  events <- loadEvents
  case List.deleteAt idx events of
    (Just updated) -> storeEvents updated
    Nothing -> pure unit

refreshEvents :: Effect Unit
refreshEvents = do
  events <- loadEvents
  storeEvents events

fireStateEvent :: Model -> Event -> App.Transition Effect Model Msg
fireStateEvent model event = {effects, model: updatedModel}
  where effects = App.lift $ pure $ StoreEvent event
        updatedModel = L.set stateL updatedState $
                       L.over _events ((:) event) $
                       model
        updatedState = St.processEvent event model.state
-- store event in local storage, fire event to update model and stats

addTimestamp :: Model -> (Maybe Instant -> Msg) -> App.Transition Effect Model Msg
addTimestamp model ctor = {effects, model}
  where effects = App.lift $ do
          nowInst <- now
          pure $ ctor (Just nowInst)

update :: Model → Msg → App.Transition Effect Model Msg
update model (Tick instant) = App.purely $
  L.set _lastUpdate instant $
  model
update model (StoreEvent event) = {effects, model}
  where effects = App.lift $ do
          storeEvent event
          pure $ DoNothing
update model (UndoEvent idx) = fireStateEvent model undo
  where event = case List.index model.events idx of
                  (Just e) -> e
                  Nothing -> unsafeThrow ("DOH: " <> show idx)
        undo = undoEvent event
update model (LogAmount id Nothing) = addTimestamp model (LogAmount id)
update model (LogAmount id (Just now)) = fireStateEvent clearedInputs progressEvent
  where amount = Input.parseStringInputUnsafe (amountInput id) model
        comment = fromMaybe "" $ Input.parseStringInputUnsafe (commentInput id) model
        clearedInputs = Input.clearInput (amountInput id) $
                        Input.clearInput (commentInput id) $ model
        progressEvent = addProgressEvent id now amount comment
update model (AddGoal) = fireStateEvent clearedInputs goalEvent
  where title = Input.parseStringInputUnsafe goalNameInput model
        startDate = dateToDateTime $ Input.parseStringInputUnsafe goalStartInput model
        endDate = dateToDateTime $ Input.parseStringInputUnsafe goalEndInput model
        target = Input.parseStringInputUnsafe goalTargetInput model
        goalEvent = addGoalEvent title startDate endDate target
        clearedInputs = Input.clearInput goalNameInput $
                        Input.clearInput goalStartInput $
                        Input.clearInput goalEndInput $
                        Input.clearInput goalTargetInput $ model
update model (RestartGoal id) = fireStateEvent clearedInputs goalEvent
  where title = Input.parseStringInputUnsafe (restartGoalNameInput id) model
        startDate = dateToDateTime $ Input.parseStringInputUnsafe (restartGoalStartInput id) model
        endDate = dateToDateTime $ Input.parseStringInputUnsafe (restartGoalEndInput id) model
        target = Input.parseStringInputUnsafe (restartGoalTargetInput id) model
        clearedInputs = Input.clearInput (restartGoalNameInput id) $
                        Input.clearInput (restartGoalStartInput id) $
                        Input.clearInput (restartGoalEndInput id) $
                        Input.clearInput (restartGoalTargetInput id) $ model
        goalEvent = restartGoalEvent id title startDate endDate target
update model (DoNothing) = App.purely model
update model (UpdateStringInput stringL input) = App.purely $ L.set stringL input model

subs :: Model -> App.Batch Sub Msg
subs model = App.lift (tickSub Tick)

app :: Page -> List Event -> Instant -> App.App Effect Sub Model Msg
app page events now = {
    render
  , update
  , subs: subs
  , init: init page events now
}

pageFromQueryParams :: M.Map String String -> Page
pageFromQueryParams queryParams =
  case M.lookup "page" queryParams of
    (Just "events") -> EventsPage
    _ -> GoalPage

runApp :: Effect Unit
runApp = do
  refreshEvents
  events <- loadEvents
  currentTime <- now
  url <- Url.getWindowUrl
  let queryParams = Url.getQueryParams url
  Console.log (show queryParams)
  let page = pageFromQueryParams queryParams
  inst <- App.makeWithSelector (basicEffect `merge` runSubscriptions) (app page events currentTime) "#app"
  inst.run
