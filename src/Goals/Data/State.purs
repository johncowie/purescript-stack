module Goals.Data.State where

import Prelude
import Data.Newtype (unwrap)
import Data.DateTime (DateTime)
import Data.DateTime.Instant (Instant, fromDateTime)
import Data.Map (filter) as M
import Data.Maybe (Maybe(..))
import Data.Foldable (elem)
import Data.Array (catMaybes)
import Utils.Lens as L
import Utils.IdMap as IdMap
import Utils.JsonDateTime (JsonDateTime)
import Goals.Data.Goal (Goal, newUnitGoal)
import Goals.Data.Goal as Goal
import Goals.Data.Event (Event(..))
-- import Effect.Exception.Unsafe (unsafeThrow)

type GoalState = IdMap.IdMap Goal

newGoalState :: GoalState
newGoalState = IdMap.new

goalFromEventRecord :: forall r. {title :: String, start :: JsonDateTime, end :: JsonDateTime, target :: Int | r} -> Goal
goalFromEventRecord r = newUnitGoal r.title (unwrap r.start) (unwrap r.end) r.target

addProgressToGoal :: DateTime -> Number -> Goal -> Goal
addProgressToGoal timeDT amount goal = L.over Goal._amountDone ((+) amountToAdd) goal
  where amountToAdd = if Goal.isCurrent time goal then amount else 0.0
        time = fromDateTime timeDT

addProgress :: IdMap.Id -> DateTime -> Number -> GoalState -> GoalState
addProgress id time amount = IdMap.update id (addProgressToGoal time amount)

processEvent :: Event -> GoalState -> GoalState
processEvent (AddGoal r) = IdMap.add (goalFromEventRecord r)
processEvent (AddProgress r) = addProgress r.id (unwrap r.time) r.amount
processEvent (RestartGoal r) = IdMap.add $ L.set Goal._predecessor (Just r.predecessor) $ goalFromEventRecord r

currentGoals :: Instant -> GoalState -> IdMap.IdMap Goal
currentGoals now = M.filter (Goal.isCurrent now)

expiredGoals :: Instant -> GoalState -> IdMap.IdMap Goal
expiredGoals now = M.filter (Goal.isExpired now)

futureGoals :: Instant -> GoalState -> IdMap.IdMap Goal
futureGoals now = M.filter (Goal.isFuture now)

hasSuccessor :: IdMap.Id -> GoalState -> Boolean
hasSuccessor id goals = elem id predecessorList
  where predecessorList = catMaybes $ map (L.view Goal._predecessor) $ IdMap.values goals
