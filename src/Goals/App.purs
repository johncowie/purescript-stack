module Goals.App where

import Prelude
import Effect (Effect)
import Utils.Lens as L
import Data.List (List(..), (:))
import Goals.Data.Goal as Goal
import Goals.Data.State as St
import Goals.Data.Stats (Stats, GoalStats, calculateStats)
import Goals.Data.Event (Event, addGoalEvent, addProgressEventV2, restartGoalEvent)
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
import Utils.DateTime (parseDate, showDate, showDayMonth)
import Utils.IdMap as IdMap
import Data.Tuple (Tuple(..))
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
  lastUpdate :: Maybe Instant,
  state :: St.GoalState,
  stats :: Stats,
  events :: List Event,
  amountInputs :: IdMap.IdMap String,
  goalForm :: GoalForm,
  inputs ::  Input.Inputs
  -- inputs :: M.Map String StringInputState
}

-- could use the Data.Default abstraction
emptyModel :: Model
emptyModel = {
  page: GoalPage,
  lastUpdate: Nothing,
  state: St.newGoalState,
  stats: IdMap.new,
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
goalNameInput = Input.stringInput _inputs "goal name" nonEmptyString "goalName"

goalTargetInput :: StringInput Model Int
goalTargetInput = Input.stringInput _inputs "target" parseInt "goalTarget"

goalStartInput :: StringInput Model DateTime
goalStartInput = Input.stringInput _inputs "start date" parseDate "goalStartDate"

goalEndInput :: StringInput Model DateTime
goalEndInput = Input.stringInput _inputs "end date" parseDate "goalEndDate"

amountInput :: IdMap.Id -> StringInput Model Number
amountInput id = Input.stringInput _inputs "amount" parseNumber ("amount-" <> show id)

commentInput :: IdMap.Id -> StringInput Model String
commentInput id = Input.stringInput _inputs "comment" anyString ("comment-" <> show id)

restartGoalNameInput :: IdMap.Id -> StringInput Model String
restartGoalNameInput id = Input.stringInput _inputs "goal name" nonEmptyString ("restartGoalTitle" <> show id)

restartGoalStartInput :: IdMap.Id -> StringInput Model DateTime
restartGoalStartInput id = Input.stringInput _inputs "start date" parseDate ("restartGoalStartDate" <> show id)

restartGoalEndInput :: IdMap.Id -> StringInput Model DateTime
restartGoalEndInput id = Input.stringInput _inputs "end date" parseDate ("restartGoalEndDate" <> show id)

restartGoalTargetInput :: IdMap.Id -> StringInput Model Int
restartGoalTargetInput id = Input.stringInput _inputs "target" parseInt ("restartGoalTarget" <> show id)

--- composing these inputs together could construct lens for SubmitForm action to take parsed vals from model

_lastUpdate :: L.Lens' Model (Maybe Instant)
_lastUpdate = L.prop (SProxy :: SProxy "lastUpdate")

_page :: L.Lens' Model Page
_page = L.prop (SProxy :: SProxy "page")

statsL :: L.Lens' Model Stats
statsL = L.prop (SProxy :: SProxy "stats")

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

-- TODO load initial state from localstorage events (events and date passed in as arg)
-- TODO can do this without passing in as args, if an event is fired off to recalculate stats..
init :: Page -> List Event -> Instant -> App.Transition Effect Model Msg
init page events dt = App.purely (emptyModel {page = page, lastUpdate = Just dt, state = state, stats = stats, events = events})
  where state = foldr St.processEvent St.newGoalState events
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
        charLength = 30
        statusClass = case isOnTrack <$> statsM of
          Nothing -> ""
          Just false -> "progress-red"
          Just true -> "progress-green"

renderOnTrackRequired :: forall a. Maybe GoalStats -> H.Html a
renderOnTrackRequired = H.text <<< show <<< fromMaybe 0 <<< map _.onTrackRequired

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
                              wrapWithClass "on-track-required" $ renderOnTrackRequired statsM,
                              progressBar statsM,
                              wrapWithClass "amount-input" $ Input.renderStringInput UpdateStringInput (amountInput id) model,
                              wrapWithClass "amount-input" $ Input.renderStringInput UpdateStringInput (commentInput id) model,
                              submitButton "Log" (LogAmount id Nothing)
                              ]
    where statsM = IdMap.get id $ L.view statsL model

renderRestartGoalForm :: IdMap.Id -> Model -> H.Html Msg
renderRestartGoalForm id model =
  H.div [P.classes ["expired-goal-form"]] $
  [ Input.renderStringInput UpdateStringInput (restartGoalNameInput id) model
  , Input.renderStringInput UpdateStringInput (restartGoalStartInput id) model
  , Input.renderStringInput UpdateStringInput (restartGoalEndInput id) model
  , Input.renderStringInput UpdateStringInput (restartGoalTargetInput id) model
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
  case model.lastUpdate of
    Nothing -> []
    (Just now) -> Array.sortWith sortF $ IdMap.toArray $ St.currentGoals (toDateTime now) $ model.state
    where sortF (Tuple id goal) = id -- Tuple (maybe 0.0 _.onTrackPerformance $ IdMap.get id $ L.view statsL model) (L.view Goal._title goal)

renderExpiredGoalList :: Model -> H.Html Msg
renderExpiredGoalList model = Keyed.div [] $ map (renderExpiredGoal model) $
  case model.lastUpdate of
    Nothing -> []
    (Just now) -> Array.filter noSuccessor $ IdMap.toArray $ St.expiredGoals (toDateTime now) $ model.state
    where noSuccessor (Tuple id goal) = not $ St.hasSuccessor id (L.view stateL model)

renderFutureGoalList :: Model -> H.Html Msg
renderFutureGoalList model = Keyed.div [] $ map (renderFutureGoal model) $
  case model.lastUpdate of
    Nothing -> []
    (Just now) -> IdMap.toArray $ St.futureGoals (toDateTime now) $ model.state

renderGoalForm :: Model -> H.Html Msg
renderGoalForm m = H.div [] [
  H.h3 [] [H.text "Add Goal"],
  inputRow "Goal name: " $ Input.renderStringInput UpdateStringInput goalNameInput m,
  inputRow "Target: " $ Input.renderStringInput UpdateStringInput goalTargetInput m,
  inputRow "Start Date: " $ Input.renderStringInput UpdateStringInput goalStartInput m,
  inputRow "End Date: " $ Input.renderStringInput UpdateStringInput goalEndInput m,
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
                       L.set statsL (calculateStats now updatedState) $
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
  L.set statsL (calculateStats instant model.state) $
  L.set _lastUpdate (Just instant) $
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
