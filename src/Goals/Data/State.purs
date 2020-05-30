module Goals.Data.State where

import Prelude
import Data.DateTime (DateTime)
import Data.DateTime.Instant (fromDateTime)
import Data.Maybe (Maybe(..))
import Data.Foldable (elem)
import Data.Array (catMaybes)
import Data.Newtype (class Newtype, wrap, unwrap)
import Data.Symbol (SProxy(..))
import Utils.Lens (type (:->))
import Utils.Lens as L
import Utils.IdMap as IdMap
import Utils.JsonDateTime (JsonDateTime)
import Goals.Data.Goal (Goal, newGoal)
import Goals.Data.Todo (Todo, newTodo)
import Goals.Data.Goal as Goal
import Goals.Data.Event (Event(..))
import Effect.Exception.Unsafe (unsafeThrow)

newtype GoalState = GoalState {
  goals :: IdMap.IdMap Goal
, todos :: IdMap.IdMap Todo
}

derive instance newtypeGoal :: Newtype GoalState _

_goals :: GoalState :-> IdMap.IdMap Goal
_goals = L._newtype >>> L.prop (SProxy :: SProxy "goals")

_todos :: GoalState :-> IdMap.IdMap Todo
_todos = L._newtype >>> L.prop (SProxy :: SProxy "todos")

newGoalState :: GoalState
newGoalState = wrap {goals, todos}
  where goals = IdMap.new
        todos = IdMap.new

lookupGoal :: IdMap.Id -> GoalState -> Maybe Goal
lookupGoal id = allGoals >>> IdMap.get id

allGoals :: GoalState -> IdMap.IdMap Goal
allGoals = unwrap >>> _.goals

goalFromEventRecord :: forall r. {title :: String, start :: JsonDateTime, end :: JsonDateTime, target :: Int | r} -> Goal
goalFromEventRecord r = newGoal r.title (unwrap r.start) (unwrap r.end) r.target

todoFromEventRecord :: forall r. {name :: String, due :: JsonDateTime, comments :: String | r} -> Todo
todoFromEventRecord r = newTodo r.name (unwrap r.due) r.comments

-- TODO move to Goal module
addProgressToGoal :: DateTime -> Number -> Goal -> Goal
addProgressToGoal timeDT amount goal = L.over Goal._amountDone ((+) amountToAdd) goal
  where amountToAdd = if Goal.isCurrent time goal then amount else 0.0
        time = fromDateTime timeDT

addProgress :: IdMap.Id -> DateTime -> Number -> GoalState -> GoalState
addProgress id time amount = L.over _goals $ IdMap.update id (addProgressToGoal time amount)

processEvent :: Event -> GoalState -> GoalState
processEvent (AddGoal r) = L.over _goals $ IdMap.add (goalFromEventRecord r)
processEvent (AddProgress r) = addProgress r.id (unwrap r.time) r.amount
processEvent (RestartGoal r) = L.over _goals $ IdMap.add $ L.set Goal._predecessor (Just r.predecessor) $ goalFromEventRecord r
processEvent (UndoEvent r) = rollbackEvent r.event
processEvent (AddTodo r) = L.over _todos $ IdMap.add (todoFromEventRecord r)
processEvent (CompletedTodo r) = unsafeThrow "implement me"

rollbackEvent :: Event -> GoalState -> GoalState
rollbackEvent (UndoEvent _) = unsafeThrow "can't undo an undo" -- TODO figure out how to deal with this
rollbackEvent (AddGoal r) = unsafeThrow "implement me" -- TODO what to do here?
rollbackEvent (RestartGoal r) = unsafeThrow "implement me" -- TODO what to do here?
rollbackEvent (AddTodo r) = unsafeThrow "implement me" -- TODO what to do here
rollbackEvent (CompletedTodo r) = unsafeThrow "implement me" -- TODO
rollbackEvent (AddProgress r) = addProgress r.id (unwrap r.time) (r.amount * -1.0)

hasSuccessor :: IdMap.Id -> GoalState -> Boolean
hasSuccessor id state = elem id predecessorList
  where predecessorList = catMaybes $ map (L.view Goal._predecessor) $ IdMap.values $ allGoals state
