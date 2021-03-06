module Goals.App where

import Prelude
import Goals.Data.Goal as Goal
import Goals.Data.Goal (Goal)
import Goals.Data.Todo as Todo
import Goals.Data.Todo (Todo)
import Goals.Data.State as St
import Goals.Data.Event (Event, addGoalEvent, addProgressEvent, restartGoalEvent, undoEvent)
import Goals.Data.Event as Event
import Spork.Html as H
import Spork.Html.Elements.Keyed as Keyed
import Spork.Html.Properties as P
import Spork.Html.Events as E
import Data.Date (Date)
import Data.DateTime (date)
import Data.DateTime.Instant (Instant)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Either (Either(..), either)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested (tuple3)
import Data.Int (fromString, toNumber, floor) as Int
import Data.Number (fromString) as Number
import Data.Map as M
import Data.Array as Array
import Data.Symbol (SProxy(..))
import Data.Newtype (wrap)
import Effect (Effect)
import Effect.Exception (Error)
import Effect.Exception.Unsafe (unsafeThrow)
import Effect.Now (now)
import Utils.NumberFormat (toDP)
import Utils.Url as Url
import Utils.Fixed as DF
import Utils.Components.Input as Input
import Utils.Components.Input (Inputs, StringInput)
import Utils.Async (async)
import Utils.DateTime (showDate, showDayMonth, dateToDateTime, nextDateTime)
import Utils.IdMap as IdMap
import Utils.AppendStore (httpAppendStore, httpSnapshotStore, ApiConfig)
import Utils.Alert (alert)
import Utils.Lens as L
import Utils.Spork.EventApp as App

data AppStatus
  = Loading
  | Loaded
  | Saving

data Msg
  = Tick Instant
  | UpdateStringInput (Input.InputSetter Model) String
  | LogAmount IdMap.Id (Maybe Instant)
  | AddGoal
  | AddTodo
  | CompletedTodo IdMap.Id (Maybe Instant)
  | RestartGoal IdMap.Id Goal.Goal
  | UndoEvent Int
  | AlertError String
  | ClearError
  | DoNothing

data Page
  = GoalPage
  | EventsPage

-- TODO remove me
type GoalForm
  = { goalName :: String
    , target :: String
    , startDate :: String
    , endDate :: String
    }

type Model
  = { error :: Maybe String
    , appStatus :: AppStatus
    , page :: Page
    , lastUpdate :: Instant
    , state :: St.GoalState
    , events :: Array Event
    , amountInputs :: IdMap.IdMap String
    , goalForm :: GoalForm
    , inputs :: Input.Inputs
    , eventAppState :: App.EventAppState
    }

type Transition
  = App.Transition Event Model Msg

-- could use the Data.Default abstraction
emptyModel :: Instant -> Model
emptyModel now =
  { error: Nothing
  , appStatus: Loaded
  , page: GoalPage
  , lastUpdate: now
  , state: St.newGoalState
  , events: []
  , amountInputs: IdMap.new
  , goalForm:
      { goalName: ""
      , target: ""
      , startDate: ""
      , endDate: ""
      }
  , inputs: M.empty
  , eventAppState: App.emptyState
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
  where
  initialVal = L.view Goal._title goal

restartGoalStartInput :: IdMap.Id -> Goal.Goal -> StringInput Model Date
restartGoalStartInput id goal = Input.stringInput_ _inputs ("restartGoalStartDate" <> show id) initialVal
  where
  initialVal = date $ L.view Goal._end goal

restartGoalEndInput :: IdMap.Id -> Goal.Goal -> StringInput Model Date
restartGoalEndInput id goal = Input.stringInput_ _inputs ("restartGoalEndDate" <> show id) initialVal
  where
  initialVal = date $ nextDateTime (L.view Goal._start goal) (L.view Goal._end goal)

restartGoalTargetInput :: IdMap.Id -> Goal.Goal -> StringInput Model Int
restartGoalTargetInput id goal = Input.stringInput_ _inputs ("restartGoalTarget" <> show id) initialVal
  where
  initialVal = L.view Goal._target goal

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

_eventAppState :: L.Lens' Model App.EventAppState
_eventAppState = L.prop (SProxy :: SProxy "eventAppState")

init :: Page -> Instant -> App.Transition Event Model Msg
init page dt = { effects: [], model, events: [] }
  where
  model = ((emptyModel dt) { page = page })

wrapWithClass :: forall a. String -> H.Html a -> H.Html a
wrapWithClass clazz node = H.div [ P.classes [ clazz ] ] [ node ]

repeatString :: Int -> String -> String
repeatString n s = case n of
  x'
    | x' <= 0 -> ""
  x' -> s <> repeatString (x' - 1) s

scalePercentage :: Int -> Number -> Int
scalePercentage points percentage = Int.floor $ percentage * (Int.toNumber points / 100.0)

isOnTrack :: Instant -> Goal -> Boolean
isOnTrack now goal = (>=) 0.0 $ Goal.onTrackRequired now goal

progressBar :: forall a. Instant -> Goal -> H.Html a
progressBar now goal =
  H.div [ P.classes [ "progress-bar" ] ]
    [ H.span [ P.classes [ statusClass ] ] [ H.text (repeatString nX "X") ]
    , H.span [ P.classes [ "progress-grey" ] ] [ H.text (repeatString nXRem "X") ]
    , H.span [] [ (H.text (repeatString nSpace " ")) ]
    ]
  where
  nX = scalePercentage charLength $ Goal.progressPercentage goal

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
submitButton isEnabled label msg = H.button [ E.onClick (E.always_ msg), P.disabled (not isEnabled) ] [ H.text label ]

-- TODO move towards each component using a lens
renderLiveGoal :: Model -> Tuple IdMap.Id Goal.Goal -> Tuple String (H.Html Msg)
renderLiveGoal model (Tuple id goal) =
  Tuple (show id)
    $ H.div []
        [ wrapWithClass "goal-label" $ H.text (show id <> ": " <> L.view Goal._title goal)
        , wrapWithClass "goal-end" $ H.text $ showDate $ date $ L.view Goal._end goal
        , wrapWithClass "amount-done" $ (H.text $ amountDoneString goal)
        , wrapWithClass "on-track-required" $ renderOnTrackRequired model.lastUpdate goal
        , progressBar model.lastUpdate goal
        , wrapWithClass "amount-input" $ Input.renderStringInput UpdateStringInput (amountInput id) "amount" model
        , wrapWithClass "amount-input" $ Input.renderStringInput UpdateStringInput (commentInput id) "comment" model
        , submitButton hasAmount "Log" (LogAmount id Nothing)
        ]
  where
  hasAmount = not $ Input.inputValue (amountInput id) model == ""

renderRestartGoalForm :: IdMap.Id -> Goal.Goal -> Model -> H.Html Msg
renderRestartGoalForm id goal model =
  H.div [ P.classes [ "expired-goal-form" ] ]
    $ [ Input.renderStringInput UpdateStringInput (restartGoalNameInput id goal) "goal name" model
      , Input.renderStringInput UpdateStringInput (restartGoalStartInput id goal) "start date" model
      , Input.renderStringInput UpdateStringInput (restartGoalEndInput id goal) "end date" model
      , Input.renderStringInput UpdateStringInput (restartGoalTargetInput id goal) "target" model
      , submitButton true "Restart Goal" (RestartGoal id goal)
      ]

renderExpiredGoal :: Model -> Tuple IdMap.Id Goal.Goal -> Tuple String (H.Html Msg)
renderExpiredGoal model (Tuple id goal) =
  Tuple (show id) $ H.div [ P.classes [ "expired-goal" ] ]
    $ [ wrapWithClass "goal-label" $ H.text $ L.view Goal._title goal
      , H.text $ showDayMonth $ L.view Goal._start goal
      , H.text $ " - "
      , H.text $ showDayMonth $ L.view Goal._end goal
      , H.text " - "
      , H.text $ show (L.view Goal._amountDone goal) <> "/" <> show (L.view Goal._target goal)
      ]
    <> if needsRestarting then
        [ renderRestartGoalForm id goal model ]
      else
        []
  where
  needsRestarting = not $ St.hasSuccessor id model.state

renderFutureGoal :: Model -> Tuple IdMap.Id Goal.Goal -> Tuple String (H.Html Msg)
renderFutureGoal model (Tuple id goal) =
  Tuple (show id) $ H.div [ P.classes [ "future-goal" ] ]
    $ [ H.text $ L.view Goal._title goal
      , H.text $ " - "
      , H.text $ showDate $ date $ L.view Goal._start goal
      , H.text $ " - "
      , H.text $ showDate $ date $ L.view Goal._end goal
      , H.text $ " - "
      , H.text $ show $ L.view Goal._target goal
      ]

renderCurrentGoalList :: Model -> H.Html Msg
renderCurrentGoalList model =
  Keyed.div [] $ map (renderLiveGoal model)
    $ Array.sortWith sortF
    $ IdMap.toArray
    $ M.filter (Goal.isInProgress now)
    $ St.allGoals model.state
  where
  sortF (Tuple id goal) = tuple3 (toFixed $ -1.0 * (Goal.requiredPercentage now goal)) (L.view Goal._end goal) (L.view Goal._title goal)

  now = model.lastUpdate

  toFixed n = case DF.fromNumber n of
    Nothing -> unsafeThrow "ARGHH"
    (Just f) -> (f :: DF.Fixed DF.P10000)

renderExpiredGoalList :: Model -> H.Html Msg
renderExpiredGoalList model =
  Keyed.div [] $ map (renderExpiredGoal model)
    $ Array.filter noSuccessor
    $ IdMap.toArray
    $ M.filter (Goal.isExpired model.lastUpdate)
    $ St.allGoals model.state
  where
  noSuccessor (Tuple id goal) = not $ St.hasSuccessor id (L.view _state model)

renderFutureGoalList :: Model -> H.Html Msg
renderFutureGoalList model =
  Keyed.div [] $ map (renderFutureGoal model)
    $ IdMap.toArray
    $ M.filter (Goal.isFuture model.lastUpdate)
    $ St.allGoals
    $ model.state

renderCurrentTodo :: Model -> Tuple IdMap.Id Todo -> Tuple String (H.Html Msg)
renderCurrentTodo model (Tuple id todo) =
  Tuple (show id) $ H.div [ P.classes [ "current-todo" ] ]
    $ [ H.text $ L.view Todo._name todo
      , H.text $ " - "
      , H.text $ showDate $ date $ L.view Todo._due todo
      , H.text $ " - "
      , submitButton true "Done" (CompletedTodo id Nothing)
      ]

renderCurrentTodoList :: Model -> H.Html Msg
renderCurrentTodoList model =
  Keyed.div [] $ map (renderCurrentTodo model)
    $ Array.filter isNotDone
    $ IdMap.toArray
    $ St.allTodos model.state
  where
  isNotDone (Tuple id todo) = not $ Todo.isDone todo

renderGoalForm :: Model -> H.Html Msg
renderGoalForm m =
  H.div []
    [ H.h3 [] [ H.text "Add Goal" ]
    , inputRow "Goal name: " $ Input.renderStringInput UpdateStringInput goalNameInput "goal name" m
    , inputRow "Target: " $ Input.renderStringInput UpdateStringInput goalTargetInput "target" m
    , inputRow "Start Date: " $ Input.renderStringInput UpdateStringInput goalStartInput "start date" m
    , inputRow "End Date: " $ Input.renderStringInput UpdateStringInput goalEndInput "end date" m
    , submitButton true "Add Goal" AddGoal
    ]
  where
  inputRow label input = H.div [] [ H.label [] [ (H.text label) ], input ]

renderTodoForm :: Model -> H.Html Msg
renderTodoForm m =
  H.div []
    [ H.h3 [] [ H.text "Add Todo" ]
    , inputRow "Todo name: " $ Input.renderStringInput UpdateStringInput todoNameInput "name" m
    , inputRow "Target: " $ Input.renderStringInput UpdateStringInput todoDueDateInput "due date" m
    , inputRow "Comments: " $ Input.renderStringInput UpdateStringInput todoCommentsInput "comments" m
    , submitButton true "Add Todo" AddTodo
    ]
  where
  inputRow label input = H.div [] [ H.label [] [ (H.text label) ], input ]

renderGoalsPage :: Model -> H.Html Msg
renderGoalsPage model =
  H.div []
    [ H.h3 [] [ H.text "Current goals" ]
    , renderCurrentGoalList model
    , H.h3 [] [ H.text "Current todos" ]
    , renderCurrentTodoList model
    , H.h3 [] [ H.text "Expired goals" ]
    , renderExpiredGoalList model
    , H.h3 [] [ H.text "Future goals" ]
    , renderFutureGoalList model
    , renderGoalForm model
    , renderTodoForm model
    ]

renderEvent :: Tuple Int Event -> H.Html Msg
renderEvent (Tuple index event) =
  H.div []
    [ H.text (show index <> ": " <> show event)
    , submitButton true "Undo" (UndoEvent index)
    ]

-- TODO Also need a button for triggering a removal.
renderEventListPage :: Model -> H.Html Msg
renderEventListPage model =
  H.div []
    [ H.h3 [] [ H.text "Events" ]
    , H.div [] $ map renderEvent $ Array.take 100 $ addIndices $ Array.fromFoldable model.events
    ]
  where
  addIndices l = Array.zip (Array.range 0 (Array.length l - 1)) l

pageTemplate :: (Model -> H.Html Msg) -> Model -> H.Html Msg
pageTemplate subPage m =
  H.div []
    [ subPage m
    ]

renderPage :: Page -> Model -> H.Html Msg
renderPage GoalPage = pageTemplate renderGoalsPage

renderPage EventsPage = pageTemplate renderEventListPage

appStatusMessage :: AppStatus -> String
appStatusMessage Loaded = " - "

appStatusMessage Loading = "Loading..."

appStatusMessage Saving = "Saving..."

renderAppStatus :: Model -> H.Html Msg
renderAppStatus model = H.div [ H.classes [ "app-status" ] ] [ H.text $ appStatusMessage $ L.view _appStatus model ]

render :: Model -> H.Html Msg
render model =
  H.div []
    [ renderAppStatus model
    , renderPage (L.view _page model) model
    ]

transition :: Model -> Msg -> Transition
transition model msg =
  { effects: [ async $ pure $ msg ]
  , model
  , events: []
  }

justEvent :: Model -> Event -> Transition
justEvent model ev = { effects: [], model, events: [ ev ] }

addTimestamp :: Model -> (Maybe Instant -> Msg) -> Transition
addTimestamp model ctor = { effects: [ effect ], model, events: [] }
  where
  effect =
    async
      $ do
          nowInst <- now
          pure $ ctor (Just nowInst)

update :: Model → Msg → Transition
update model (Tick instant) =
  App.purely
    $ L.set _lastUpdate instant
    $ model

update model (UndoEvent idx) = case Array.index model.events idx of
  Nothing -> transition model (AlertError $ "No event with ID: " <> show idx)
  (Just e) ->
    { effects: []
    , model
    , events: [ undoEvent e ]
    }

update model (LogAmount id Nothing) = addTimestamp model (LogAmount id)

update model (LogAmount id (Just now)) = either alertError (justEvent clearedInputs) progressEvent
  where
  clearedInputs =
    Input.clearInput (amountInput id)
      $ Input.clearInput (commentInput id)
      $ model

  progressEvent =
    addProgressEvent id now
      <$> Input.parseStringInput (amountInput id) model
      <*> (fromMaybe "" <$> Input.parseStringInput (commentInput id) model)

  alertError err = transition model (AlertError err)

update model (AddGoal) = either alertError (justEvent clearedInputs) goalEvent
  where
  clearedInputs =
    Input.clearInput goalNameInput
      $ Input.clearInput goalStartInput
      $ Input.clearInput goalEndInput
      $ Input.clearInput goalTargetInput
      $ model

  goalEvent =
    addGoalEvent
      <$> Input.parseStringInput goalNameInput model
      <*> (dateToDateTime <$> Input.parseStringInput goalStartInput model)
      <*> (dateToDateTime <$> Input.parseStringInput goalEndInput model)
      <*> Input.parseStringInput goalTargetInput model

  alertError err = transition model (AlertError err)

update model (AddTodo) = either alertError (justEvent clearedInputs) todoEvent
  where
  todoEvent =
    Event.addTodoEvent
      <$> Input.parseStringInput todoNameInput model
      <*> (dateToDateTime <$> Input.parseStringInput todoDueDateInput model)
      <*> (fromMaybe "" <$> Input.parseStringInput todoCommentsInput model)

  clearedInputs =
    Input.clearInput todoNameInput
      $ Input.clearInput todoDueDateInput
      $ Input.clearInput todoCommentsInput
      $ model

  alertError err = transition model (AlertError err)

update model (CompletedTodo id Nothing) = addTimestamp model (CompletedTodo id)

update model (CompletedTodo id (Just ts)) = justEvent model (Event.completedTodoEvent id ts)

update model (RestartGoal id goal) = either alertError (justEvent clearedInputs) goalEvent
  where
  clearedInputs =
    Input.clearInput (restartGoalNameInput id goal)
      $ Input.clearInput (restartGoalStartInput id goal)
      $ Input.clearInput (restartGoalEndInput id goal)
      $ Input.clearInput (restartGoalTargetInput id goal)
      $ model

  goalEvent =
    restartGoalEvent id
      <$> Input.parseStringInput (restartGoalNameInput id goal) model
      <*> (dateToDateTime <$> Input.parseStringInput (restartGoalStartInput id goal) model)
      <*> (dateToDateTime <$> Input.parseStringInput (restartGoalEndInput id goal) model)
      <*> Input.parseStringInput (restartGoalTargetInput id goal) model

  alertError err = transition model (AlertError err)

update model (DoNothing) = App.purely model

update model (UpdateStringInput stringL input) = App.purely $ Input.updateInput stringL input model

update model (AlertError errorMsg) = { effects: [ effect ], model, events: [] }
  where
  effect = do
    async (alert errorMsg)
    pure DoNothing

update model (ClearError) = App.purely $ L.set _error Nothing model

app :: AppConfig -> Page -> Instant -> App.App St.GoalState Event Model Msg
app config page now =
  { render
  , update
  , init: init page now
  , tick: Just (Tuple Tick (wrap 1000.0))
  , _state
  , _eventAppState
  , eventStore: httpAppendStore config.apiConfig "goals"
  , snapshotStore: httpSnapshotStore config.apiConfig "goals"
  , reducer: St.processEvent
  }

pageFromQueryParams :: M.Map String String -> Page
pageFromQueryParams queryParams = case M.lookup "page" queryParams of
  (Just "events") -> EventsPage
  _ -> GoalPage

affErrorHandler :: Error -> Effect Unit
affErrorHandler err = alert (show err)

type AppConfig
  = { apiConfig :: ApiConfig
    }

mkConfig :: ApiConfig -> AppConfig
mkConfig = { apiConfig: _ }

runApp :: AppConfig -> Effect Unit
runApp conf = do
  currentTime <- now
  url <- Url.getWindowUrl
  let
    queryParams = Url.getQueryParams url
  let
    page = pageFromQueryParams queryParams
  inst <- App.makeWithSelector (app conf page currentTime) "#app"
  inst.run
