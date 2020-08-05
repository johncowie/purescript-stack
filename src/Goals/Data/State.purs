module Goals.Data.State where

import Prelude
import Data.DateTime (DateTime)
import Data.DateTime.Instant (Instant, fromDateTime)
import Data.Maybe (Maybe(..))
import Data.Foldable (elem)
import Data.Array (catMaybes)
import Data.Newtype (class Newtype, wrap, unwrap)
import Data.Symbol (SProxy(..))
import Data.Map as M
import Data.Tuple (Tuple(..))
import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Encode (class EncodeJson)
import Utils.Lens (type (:->))
import Utils.Lens as L
import Utils.IdMap as IdMap
import Utils.JsonDateTime (JsonDateTime)
import Utils.JsonNewtype (decodeNewtype, encodeNewtype)
import Goals.Data.Goal (Goal, newGoal)
import Goals.Data.Goal as Goal
import Goals.Data.Todo (Todo, newTodo)
import Goals.Data.Todo as Todo
import Goals.Data.Event (Event(..))
import Effect.Exception.Unsafe (unsafeThrow)

newtype GoalState
  = GoalState
  { goals :: IdMap.IdMap Goal
  , todos :: IdMap.IdMap Todo
  }

derive instance newtypeGoal :: Newtype GoalState _

instance decodeJsonGoalState :: DecodeJson GoalState where
  decodeJson = decodeNewtype "GoalState"

instance encodeJsonGoalState :: EncodeJson GoalState where
  encodeJson = encodeNewtype

_goals :: GoalState :-> IdMap.IdMap Goal
_goals = L._newtype >>> L.prop (SProxy :: SProxy "goals")

_todos :: GoalState :-> IdMap.IdMap Todo
_todos = L._newtype >>> L.prop (SProxy :: SProxy "todos")

newGoalState :: GoalState
newGoalState = wrap { goals, todos }
  where
  goals = IdMap.new

  todos = IdMap.new

lookupGoal :: IdMap.Id -> GoalState -> Maybe Goal
lookupGoal id = allGoals >>> IdMap.get id

allGoals :: GoalState -> IdMap.IdMap Goal
allGoals = unwrap >>> _.goals

allTodos :: GoalState -> IdMap.IdMap Todo
allTodos = unwrap >>> _.todos

goalFromEventRecord :: forall r. { title :: String, start :: JsonDateTime, end :: JsonDateTime, target :: Int | r } -> Goal
goalFromEventRecord r = newGoal r.title (unwrap r.start) (unwrap r.end) r.target

todoFromEventRecord :: forall r. { name :: String, due :: JsonDateTime, comments :: String | r } -> Todo
todoFromEventRecord r = newTodo r.name (unwrap r.due) r.comments

-- TODO move to Goal module
addProgressToGoal :: DateTime -> Number -> Goal -> Goal
addProgressToGoal timeDT amount goal = L.over Goal._amountDone ((+) amountToAdd) goal
  where
  amountToAdd = if Goal.isCurrent time goal then amount else 0.0

  time = fromDateTime timeDT

addProgress :: IdMap.Id -> DateTime -> Number -> GoalState -> GoalState
addProgress id time amount = L.over _goals $ IdMap.update id (addProgressToGoal time amount)

addGoal :: Goal -> GoalState -> GoalState
addGoal goal = L.over _goals (IdMap.add goal)

restartGoal :: IdMap.Id -> Goal -> GoalState -> GoalState
restartGoal predecessor goal = L.over _goals addSuccessor
  where
  addSuccessor goals =
    let
      (Tuple successorId updatedGoals) = IdMap.addReturnId successor goals
    in
      IdMap.update predecessor (L.set Goal._successor (Just successorId)) updatedGoals

  successor = L.set Goal._predecessor (Just predecessor) goal

addTodo :: Todo -> GoalState -> GoalState
addTodo todo = L.over _todos $ IdMap.add todo

pruneGoals :: GoalState -> GoalState
pruneGoals = L.over _goals $ M.filter (not <<< Goal.hasSuccessor)

processEvent :: Event -> GoalState -> GoalState
processEvent (AddGoal r) = addGoal (goalFromEventRecord r) >>> pruneGoals

processEvent (AddProgress r) = addProgress r.id (unwrap r.time) r.amount

processEvent (RestartGoal r) = restartGoal r.predecessor (goalFromEventRecord r) >>> pruneGoals

processEvent (UndoEvent r) = case rollbackEvent r.event of
  (Just f) -> f
  Nothing -> unsafeThrow "rollback for this event type is unimplemented"

processEvent (AddTodo r) = addTodo (todoFromEventRecord r)

processEvent (CompletedTodo r) = L.over (_todos >>> L._mapValMaybe r.id) (map (Todo.markAsDone (unwrap r.completedAt)))

rollbackEvent :: Event -> Maybe (GoalState -> GoalState)
rollbackEvent (UndoEvent _) = Nothing

rollbackEvent (AddGoal r) = Nothing

rollbackEvent (RestartGoal r) = Nothing

rollbackEvent (AddTodo r) = Nothing

rollbackEvent (CompletedTodo r) = Nothing

rollbackEvent (AddProgress r) = Just $ addProgress r.id (unwrap r.time) (r.amount * -1.0)

hasSuccessor :: IdMap.Id -> GoalState -> Boolean
hasSuccessor id state = elem id predecessorList
  where
  predecessorList = catMaybes $ map (L.view Goal._predecessor) $ IdMap.values $ allGoals state
