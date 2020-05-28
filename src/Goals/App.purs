module Goals.App where

import Prelude
import Utils.Lens as L
import Goals.Data.Goal as Goal
import Goals.Data.Goal (Goal)
import Goals.Data.State as St
import Goals.Data.Event (Event, addGoalEvent, addProgressEvent, restartGoalEvent, undoEvent)
import Goals.Data.Event as Event
import Data.Date (Date)
import Data.DateTime (date)
import Data.DateTime.Instant (Instant)
import Spork.App as App
import Spork.Html as H
import Spork.Html.Elements.Keyed as Keyed
import Spork.Html.Properties as P
import Spork.Html.Events as E
import Spork.Interpreter (basicAff, merge)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Either (Either(..), either)
import Data.Foldable (foldr)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested (tuple3)
import Data.Int (fromString, toNumber, floor) as Int
import Data.Number (fromString) as Number
import Data.Map as M
import Data.Array as Array
import Data.Symbol (SProxy(..))
import Effect (Effect)
import Effect.Exception (Error)
import Effect.Exception.Unsafe (unsafeThrow)
import Effect.Now (now)
import Effect.Aff (Aff)

import Utils.NumberFormat (toDP)
import Utils.Url as Url
import Utils.Fixed as DF
import Utils.Components.Input as Input
import Utils.Components.Input (Inputs, StringInput)
import Utils.Async (async)
import Utils.Spork.TimerSubscription (runSubscriptions, tickSub, Sub)
import Utils.DateTime (showDate, showDayMonth, dateToDateTime, nextDateTime)
import Utils.IdMap as IdMap
import Utils.AppendStore (AppendStore, httpAppendStore)
import Utils.Alert (alert)

data AppStatus = Loading | Loaded | Saving

data Msg = Tick Instant |
           UpdateStringInput (Input.InputSetter Model) String |
           LogAmount IdMap.Id (Maybe Instant) |
           AddGoal |
           AddTodo |
           RestartGoal IdMap.Id Goal.Goal |
           StoreEvent Event |
           StoredEvent |
           UndoEvent Int |
           LoadEvents |
           LoadedEvents (Array Event) |
           AlertError String |
           ClearError |
           DoNothing

data Page = GoalPage | EventsPage

type GoalForm = {
  goalName :: String,
  target :: String,
  startDate :: String,
  endDate :: String
}

type Model = {
  error :: Maybe String,
  appStatus :: AppStatus,
  page :: Page,
  lastUpdate :: Instant,
  state :: St.GoalState,
  events :: Array Event,
  amountInputs :: IdMap.IdMap String,
  goalForm :: GoalForm,
  inputs ::  Input.Inputs
  -- inputs :: M.Map String StringInputState
}

-- could use the Data.Default abstraction
emptyModel :: Instant -> Model
emptyModel now = {
  error: Nothing,
  appStatus: Loaded,
  page: GoalPage,
  lastUpdate: now,
  state: St.newGoalState,
  events: [],
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

todoNameInput :: StringInput Model String
todoNameInput = Input.stringInput _inputs "todoName"

todoDueDateInput :: StringInput Model Date
todoDueDateInput = Input.stringInput _inputs "todoDue"

todoCommentsInput :: StringInput Model (Maybe String)
todoCommentsInput = Input.stringInput _inputs "todoComments"

amountInput :: IdMap.Id -> StringInput Model Number
amountInput id = Input.stringInput _inputs ("amount-" <> show id)

commentInput :: IdMap.Id -> StringInput Model (Maybe String)
commentInput id = Input.stringInput _inputs ("comment-" <> show id)

restartGoalNameInput :: IdMap.Id -> Goal.Goal -> StringInput Model String
restartGoalNameInput id goal = Input.stringInput_ _inputs ("restartGoalTitle" <> show id) initialVal
  where initialVal = L.view Goal._title goal

restartGoalStartInput :: IdMap.Id -> Goal.Goal -> StringInput Model Date
restartGoalStartInput id goal = Input.stringInput_ _inputs ("restartGoalStartDate" <> show id) initialVal
  where initialVal = date $ L.view Goal._end goal

restartGoalEndInput :: IdMap.Id -> Goal.Goal -> StringInput Model Date
restartGoalEndInput id goal = Input.stringInput_ _inputs ("restartGoalEndDate" <> show id) initialVal
  where initialVal = date $ nextDateTime (L.view Goal._start goal) (L.view Goal._end goal)

restartGoalTargetInput :: IdMap.Id -> Goal.Goal -> StringInput Model Int
restartGoalTargetInput id goal = Input.stringInput_ _inputs ("restartGoalTarget" <> show id) initialVal
  where initialVal = L.view Goal._target goal

--- composing these inputs together could construct lens for SubmitForm action to take parsed vals from model

_lastUpdate :: L.Lens' Model Instant
_lastUpdate = L.prop (SProxy :: SProxy "lastUpdate")

_page :: L.Lens' Model Page
_page = L.prop (SProxy :: SProxy "page")

_state :: L.Lens' Model St.GoalState
_state = L.prop (SProxy :: SProxy "state")

_events :: L.Lens' Model (Array Event)
_events = L.prop (SProxy :: SProxy "events")

_inputs :: L.Lens' Model Inputs
_inputs = L.prop (SProxy :: SProxy "inputs")

_appStatus :: L.Lens' Model AppStatus
_appStatus = L.prop (SProxy :: SProxy "appStatus")

_error :: L.Lens' Model (Maybe String)
_error = L.prop (SProxy :: SProxy "error")

mapValL :: forall k v. (Ord k) => v -> k -> L.Lens' (M.Map k v) v
mapValL default id = L.lens get set
  where get m = fromMaybe default $ M.lookup id m
        set m v = M.insert id v m

init :: Page -> Instant -> App.Transition Aff Model Msg
init page dt = {effects, model}
  where model = ((emptyModel dt) {page = page})
        effects = pure LoadEvents

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

submitButton :: forall m. Boolean -> String -> m -> H.Html m
submitButton isEnabled label msg = H.button [E.onClick (E.always_ msg), P.disabled (not isEnabled)] [H.text label]

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
                              submitButton hasAmount "Log" (LogAmount id Nothing)
                              ]
  where hasAmount = not $ Input.inputValue (amountInput id) model == ""

renderRestartGoalForm :: IdMap.Id -> Goal.Goal -> Model -> H.Html Msg
renderRestartGoalForm id goal model =
  H.div [P.classes ["expired-goal-form"]] $
  [ Input.renderStringInput UpdateStringInput (restartGoalNameInput id goal) "goal name" model
  , Input.renderStringInput UpdateStringInput (restartGoalStartInput id goal) "start date" model
  , Input.renderStringInput UpdateStringInput (restartGoalEndInput id goal) "end date" model
  , Input.renderStringInput UpdateStringInput (restartGoalTargetInput id goal) "target" model
  , submitButton true "Restart Goal" (RestartGoal id goal)
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
    then [renderRestartGoalForm id goal model]
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
    Array.sortWith sortF $ IdMap.toArray $ M.filter (Goal.isInProgress now) $ St.allGoals model.state
    where sortF (Tuple id goal) = tuple3 (toFixed $ -1.0 * (Goal.requiredPercentage now goal)) (L.view Goal._end goal) (L.view Goal._title goal)
          now = model.lastUpdate
          toFixed n = case DF.fromNumber n of
                        Nothing -> unsafeThrow "ARGHH"
                        (Just f) -> (f :: DF.Fixed DF.P10000)

renderExpiredGoalList :: Model -> H.Html Msg
renderExpiredGoalList model = Keyed.div [] $ map (renderExpiredGoal model) $
  Array.filter noSuccessor $ IdMap.toArray $ M.filter (Goal.isExpired model.lastUpdate) $ St.allGoals model.state
    where noSuccessor (Tuple id goal) = not $ St.hasSuccessor id (L.view _state model)

renderFutureGoalList :: Model -> H.Html Msg
renderFutureGoalList model = Keyed.div [] $ map (renderFutureGoal model) $
  IdMap.toArray $ M.filter (Goal.isFuture model.lastUpdate) $ St.allGoals $ model.state

renderGoalForm :: Model -> H.Html Msg
renderGoalForm m = H.div [] [
  H.h3 [] [H.text "Add Goal"],
  inputRow "Goal name: " $ Input.renderStringInput UpdateStringInput goalNameInput "goal name" m,
  inputRow "Target: " $ Input.renderStringInput UpdateStringInput goalTargetInput "target" m,
  inputRow "Start Date: " $ Input.renderStringInput UpdateStringInput goalStartInput "start date" m,
  inputRow "End Date: " $ Input.renderStringInput UpdateStringInput goalEndInput "end date" m,
  submitButton true "Add Goal" AddGoal
] where inputRow label input = H.div [] [H.label [] [(H.text label)], input]

renderTodoForm :: Model -> H.Html Msg
renderTodoForm m = H.div [] [
  H.h3 [] [H.text "Add Todo"],
  inputRow "Todo name: " $ Input.renderStringInput UpdateStringInput todoNameInput "name" m
, inputRow "Target: " $ Input.renderStringInput UpdateStringInput todoDueDateInput "due date" m
, inputRow "Comments: " $ Input.renderStringInput UpdateStringInput todoCommentsInput "comments" m
, submitButton true "Add Todo" AddTodo
] where inputRow label input = H.div [] [H.label [] [(H.text label)], input]

renderGoalsPage :: Model -> H.Html Msg
renderGoalsPage model = H.div [] [  H.h3 [] [H.text "Current goals"]
                                  , renderCurrentGoalList model
                                  , H.h3 [] [H.text "Expired goals"]
                                  , renderExpiredGoalList model
                                  , H.h3 [] [H.text "Future goals"]
                                  , renderFutureGoalList model
                                  , renderGoalForm model
                                  , renderTodoForm model ]

renderEvent :: Tuple Int Event -> H.Html Msg
renderEvent (Tuple index event) =
  H.div [] [ H.text (show index <> ": " <> show event)
           , submitButton true "Undo" (UndoEvent index)
           ]
-- TODO Also need a button for triggering a removal.

renderEventListPage :: Model -> H.Html Msg
renderEventListPage model =
  H.div [] [
    H.h3 [] [H.text "Events"],
    H.div [] $ map renderEvent $ Array.take 100 $ addIndices $ Array.fromFoldable model.events
  ]
  where addIndices l = Array.zip (Array.range 0 (Array.length l - 1)) l

pageTemplate :: (Model -> H.Html Msg) -> Model -> H.Html Msg
pageTemplate subPage m =
  H.div [] [
    subPage m
  ]

renderPage :: Page -> Model -> H.Html Msg
renderPage GoalPage = pageTemplate renderGoalsPage
renderPage EventsPage = pageTemplate renderEventListPage

appStatusMessage :: AppStatus -> String
appStatusMessage Loaded = " - "
appStatusMessage Loading = "Loading..."
appStatusMessage Saving = "Saving..."

renderAppStatus :: Model -> H.Html Msg
renderAppStatus model = H.div [H.classes ["app-status"]] [H.text $ appStatusMessage $ L.view _appStatus model]

render :: Model -> H.Html Msg
render model = H.div [] [
  renderAppStatus model
, renderPage (L.view _page model) model
]

store :: AppendStore Event
-- store = localStorageAppendStore "goals"
store = httpAppendStore "goals"
-- store = syncAppendStore "goals"

loadEvents :: Aff (Array Event)
loadEvents = do
  eventsE <- store.retrieveAll
  pure $ either (unsafeThrow <<< show) identity eventsE

storeEvent :: Event -> Aff Unit
storeEvent event = void $ store.append event

fireStateEvent :: Model -> Event -> App.Transition Aff Model Msg
fireStateEvent model event = transition updatedModel (StoreEvent event)
  where updatedModel = L.set _state updatedState $
                       L.over _events ((<>) [event]) $
                       model
        updatedState = St.processEvent event model.state
-- store event in local storage, fire event to update model and stats

transition :: Model -> Msg -> App.Transition Aff Model Msg
transition model msg = { effects: App.lift $ async $ pure $ msg
                        , model}

addTimestamp :: Model -> (Maybe Instant -> Msg) -> App.Transition Aff Model Msg
addTimestamp model ctor = {effects, model}
  where effects = App.lift $ async $ do
          nowInst <- now
          pure $ ctor (Just nowInst)

update :: Model → Msg → App.Transition Aff Model Msg
update model (Tick instant) = App.purely $
  L.set _lastUpdate instant $
  model
update model LoadEvents = {effects, model: L.set _appStatus Loading model}
  where effects = App.lift $ do
          LoadedEvents <$> loadEvents
-- update model LoadEvents = App.purely $ L.set _appStatus Loading model
update model (LoadedEvents events) = App.purely $
  L.set _appStatus Loaded $
  L.set _events events $
  L.set _state (foldr St.processEvent St.newGoalState events) $
  model
update model (StoreEvent event) = {effects, model: L.set _appStatus Saving model}
  where effects = App.lift $ do
          storeEvent event
          pure $ StoredEvent
update model StoredEvent = App.purely $ L.set _appStatus Loaded model
update model (UndoEvent idx) = case Array.index model.events idx of
                (Just e) -> fireStateEvent model (undoEvent e)
                Nothing -> transition model (AlertError $ "No event with ID: " <> show idx)
update model (LogAmount id Nothing) = addTimestamp model (LogAmount id)
update model (LogAmount id (Just now)) = either alertError (fireStateEvent clearedInputs) progressEvent
  where clearedInputs = Input.clearInput (amountInput id) $
                        Input.clearInput (commentInput id) $ model
        progressEvent = addProgressEvent id now <$>
                        Input.parseStringInput (amountInput id) model <*>
                        (fromMaybe "" <$> Input.parseStringInput (commentInput id) model)
        alertError err = transition model (AlertError err)
update model (AddGoal) = either alertError (fireStateEvent clearedInputs) goalEvent
  where clearedInputs = Input.clearInput goalNameInput $
                        Input.clearInput goalStartInput $
                        Input.clearInput goalEndInput $
                        Input.clearInput goalTargetInput $ model
        goalEvent = addGoalEvent <$>
                    Input.parseStringInput goalNameInput model <*>
                    (dateToDateTime <$> Input.parseStringInput goalStartInput model) <*>
                    (dateToDateTime <$> Input.parseStringInput goalEndInput model) <*>
                    Input.parseStringInput goalTargetInput model
        alertError err = transition model (AlertError err)
update model (AddTodo) = either alertError (fireStateEvent clearedInputs) todoEvent
  where todoEvent = Event.addTodoEvent <$>
                    Input.parseStringInput todoNameInput model <*>
                    (dateToDateTime <$> Input.parseStringInput todoDueDateInput model) <*>
                    (fromMaybe "" <$> Input.parseStringInput todoCommentsInput model)
        clearedInputs = Input.clearInput todoNameInput $
                        Input.clearInput todoDueDateInput $
                        Input.clearInput todoCommentsInput $ model
        alertError err = transition model (AlertError err)
update model (RestartGoal id goal) = either alertError (fireStateEvent clearedInputs) goalEvent
  where clearedInputs = Input.clearInput (restartGoalNameInput id goal) $
                        Input.clearInput (restartGoalStartInput id goal) $
                        Input.clearInput (restartGoalEndInput id goal) $
                        Input.clearInput (restartGoalTargetInput id goal) $ model
        goalEvent = restartGoalEvent id <$>
                    Input.parseStringInput (restartGoalNameInput id goal) model <*>
                    (dateToDateTime <$> Input.parseStringInput (restartGoalStartInput id goal) model) <*>
                    (dateToDateTime <$> Input.parseStringInput (restartGoalEndInput id goal) model) <*>
                    Input.parseStringInput (restartGoalTargetInput id goal) model
        alertError err = transition model (AlertError err)
update model (DoNothing) = App.purely model
update model (UpdateStringInput stringL input) = App.purely $ Input.updateInput stringL input model
update model (AlertError errorMsg) = {effects, model}
  where effects = App.lift do
          async (alert errorMsg)
          pure DoNothing
update model (ClearError) = App.purely $ L.set _error Nothing model

subs :: Model -> App.Batch Sub Msg
subs model = App.lift (tickSub Tick)

app :: Page -> Instant -> App.App Aff Sub Model Msg
app page now = {
    render
  , update
  , subs: subs
  , init: init page now
}

pageFromQueryParams :: M.Map String String -> Page
pageFromQueryParams queryParams =
  case M.lookup "page" queryParams of
    (Just "events") -> EventsPage
    _ -> GoalPage

affErrorHandler :: Error -> Effect Unit
affErrorHandler err = alert (show err)

runApp :: Effect Unit
runApp = do
  currentTime <- now
  url <- Url.getWindowUrl
  let queryParams = Url.getQueryParams url
  let page = pageFromQueryParams queryParams
  inst <- App.makeWithSelector (basicAff affErrorHandler `merge` runSubscriptions) (app page currentTime) "#app"
  inst.run
