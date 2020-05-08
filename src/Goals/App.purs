module Goals.App where

import Prelude
import Effect (Effect)
import Utils.Lens as L
import Data.List (List(..), (:))
import Goals.Data.Goal as Goal
import Goals.Data.Goal (Goal)
import Goals.Data.State as St
import Goals.Data.Event (Event, addGoalEvent, addProgressEventV2, restartGoalEvent)
import Data.DateTime (DateTime)
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
import Utils.DateTime (parseDate, showDate, showDayMonth)
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
import Effect.Console as Console
import Utils.Components.Input as Input
import Utils.Components.Input (StringInput)
import Data.Array as Array

data Msg = Tick Instant |
           UpdateStringInput (L.Lens' Model String) String |
           LogAmount IdMap.Id (Maybe Instant) |
           AddGoal (Maybe Instant) |
           RestartGoal IdMap.Id (Maybe Instant) |
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

nonEmptyString :: String -> Either String String
nonEmptyString "" = Left "Can't be empty"
nonEmptyString s = Right s

anyString :: String -> Either String String
anyString s = Right s

goalNameInput :: StringInput Model String
goalNameInput = Input.stringInput _inputs nonEmptyString "goalName"

goalTargetInput :: StringInput Model Int
goalTargetInput = Input.stringInput _inputs parseInt "goalTarget"

goalStartInput :: StringInput Model DateTime
goalStartInput = Input.stringInput _inputs parseDate "goalStartDate"

goalEndInput :: StringInput Model DateTime
goalEndInput = Input.stringInput _inputs parseDate "goalEndDate"

amountInput :: IdMap.Id -> StringInput Model Number
amountInput id = Input.stringInput _inputs parseNumber ("amount-" <> show id)

commentInput :: IdMap.Id -> StringInput Model String
commentInput id = Input.stringInput _inputs anyString ("comment-" <> show id)

restartGoalNameInput :: IdMap.Id -> StringInput Model String
restartGoalNameInput id = Input.stringInput _inputs nonEmptyString ("restartGoalTitle" <> show id)

restartGoalStartInput :: IdMap.Id -> StringInput Model DateTime
restartGoalStartInput id = Input.stringInput _inputs parseDate ("restartGoalStartDate" <> show id)

restartGoalEndInput :: IdMap.Id -> StringInput Model DateTime
restartGoalEndInput id = Input.stringInput _inputs parseDate ("restartGoalEndDate" <> show id)

restartGoalTargetInput :: IdMap.Id -> StringInput Model Int
restartGoalTargetInput id = Input.stringInput _inputs parseInt ("restartGoalTarget" <> show id)

--- composing these inputs together could construct lens for SubmitForm action to take parsed vals from model

_lastUpdate :: L.Lens' Model Instant
_lastUpdate = L.prop (SProxy :: SProxy "lastUpdate")

_page :: L.Lens' Model Page
_page = L.prop (SProxy :: SProxy "page")

stateL :: L.Lens' Model St.GoalState
stateL = L.prop (SProxy :: SProxy "state")

_events :: L.Lens' Model (List Event)
_events = L.prop (SProxy :: SProxy "events")

_inputs :: L.Lens' Model (M.Map String String)
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
                              wrapWithClass "goal-end" $ H.text $ showDate $ L.view Goal._end goal,
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
  , submitButton "Restart Goal" (RestartGoal id Nothing)
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
    H.text $ showDate $ L.view Goal._start goal,
    H.text $ " - ",
    H.text $ showDate $ L.view Goal._end goal,
    H.text $ " - ",
    H.text $ show $ L.view Goal._target goal
  ]

renderCurrentGoalList :: Model -> H.Html Msg
renderCurrentGoalList model = Keyed.div [] $ map (renderLiveGoal model) $
    Array.sortWith sortF $ IdMap.toArray $ St.currentGoals now $ model.state
    where sortF (Tuple id goal) = tuple3 (-1.0 * (Goal.requiredPercentage now goal)) (L.view Goal._end goal) (L.view Goal._title goal)
          now = model.lastUpdate

renderExpiredGoalList :: Model -> H.Html Msg
renderExpiredGoalList model = Keyed.div [] $ map (renderExpiredGoal model) $
  Array.filter noSuccessor $ IdMap.toArray $ St.expiredGoals model.lastUpdate $ model.state
    where noSuccessor (Tuple id goal) = not $ St.hasSuccessor id (L.view stateL model)

renderFutureGoalList :: Model -> H.Html Msg
renderFutureGoalList model = Keyed.div [] $ map (renderFutureGoal model) $
  IdMap.toArray $ St.futureGoals model.lastUpdate $ model.state

renderGoalForm :: Model -> H.Html Msg
renderGoalForm m = H.div [] [
  H.h3 [] [H.text "Add Goal"],
  inputRow "Goal name: " $ Input.renderStringInput UpdateStringInput goalNameInput "goal name" m,
  inputRow "Target: " $ Input.renderStringInput UpdateStringInput goalTargetInput "target" m,
  inputRow "Start Date: " $ Input.renderStringInput UpdateStringInput goalStartInput "start date" m,
  inputRow "End Date: " $ Input.renderStringInput UpdateStringInput goalEndInput "end date" m,
  submitButton "Add Goal" (AddGoal Nothing)
] where inputRow label input = H.div [] [H.label [] [(H.text label)], input]

renderGoalsPage :: Model -> H.Html Msg
renderGoalsPage model = H.div [] [H.h3 [] [H.text "Current goals"],
                                  renderCurrentGoalList model,
                                  H.h3 [] [H.text "Expired goals"],
                                  renderExpiredGoalList model,
                                  H.h3 [] [H.text "Future goals"],
                                  renderFutureGoalList model,
                                  renderGoalForm model]

renderEvent :: Event -> H.Html Msg
renderEvent event = H.div [] [H.text "something something"]
-- TODO convert event to javascript and write as text
-- TODO Also need a button for triggering a removal.

renderEventListPage :: Model -> H.Html Msg
renderEventListPage model =
  H.div [] [
    H.h3 [] [H.text "Events"],
    H.div [] $ map renderEvent $ Array.fromFoldable model.events
  ]

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

refreshEvents :: Effect Unit
refreshEvents = do
  events <- loadEvents
  storeEvents events

fireStateEvent :: Instant -> Model -> Event -> App.Transition Effect Model Msg
fireStateEvent now model event = {effects, model: updatedModel}
  where effects = App.lift $ do
          storeEvent event
          pure $ DoNothing
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
update model (Tick instant) =  App.purely $
  L.set _lastUpdate instant $
  model
update model (LogAmount id Nothing) = addTimestamp model (LogAmount id)
update model (LogAmount id (Just now)) = fireStateEvent now clearedInputs progressEvent
  where amount = Input.parseStringInputUnsafe (amountInput id) model
        comment = Input.parseStringInputUnsafe (commentInput id) model
        clearedInputs = L.set (amountInput id).lens "" $
                        L.set (commentInput id).lens "" $ model
        progressEvent = addProgressEventV2 id now amount comment
update model (AddGoal Nothing) = addTimestamp model AddGoal
update model (AddGoal (Just now)) = fireStateEvent now clearedInputs goalEvent
  where title = Input.parseStringInputUnsafe goalNameInput model
        startDate = Input.parseStringInputUnsafe goalStartInput model
        endDate = Input.parseStringInputUnsafe goalEndInput model
        target = Input.parseStringInputUnsafe goalTargetInput model
        goalEvent = addGoalEvent title startDate endDate target
        clearedInputs = Input.clearInput goalNameInput $
                        Input.clearInput goalStartInput $
                        Input.clearInput goalEndInput $
                        Input.clearInput goalTargetInput $ model
update model (RestartGoal id Nothing) = addTimestamp model (RestartGoal id)
update model (RestartGoal id (Just now)) = fireStateEvent now clearedInputs goalEvent
  where title = Input.parseStringInputUnsafe (restartGoalNameInput id) model
        startDate = Input.parseStringInputUnsafe (restartGoalStartInput id) model
        endDate = Input.parseStringInputUnsafe (restartGoalEndInput id) model
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
