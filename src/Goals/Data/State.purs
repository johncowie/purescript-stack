module Goals.Data.State where

import Prelude
import Data.Newtype (unwrap)
import Data.DateTime (DateTime)
import Data.Map (filter)
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

goalFromEventRecord :: {title :: String, start :: JsonDateTime, end :: JsonDateTime, target :: Int} -> Goal
goalFromEventRecord r = newUnitGoal r.title (unwrap r.start) (unwrap r.end) r.target

addProgressToGoal :: DateTime -> Number -> Goal -> Goal
addProgressToGoal time amount goal = L.over Goal._amountDone ((+) amountToAdd) goal
  where amountToAdd = if Goal.isCurrent time goal then amount else 0.0

addProgress :: IdMap.Id -> DateTime -> Number -> GoalState -> GoalState
addProgress id time amount = IdMap.update id (addProgressToGoal time amount)

processEvent :: Event -> GoalState -> GoalState
processEvent (AddGoal r) = IdMap.add (goalFromEventRecord r)
processEvent (AddProgress r) = addProgress r.id (unwrap r.time) r.amount

currentGoals :: DateTime -> GoalState -> IdMap.IdMap Goal
currentGoals now = filter (Goal.isCurrent now)

expiredGoals :: DateTime -> GoalState -> IdMap.IdMap Goal
expiredGoals now = filter (Goal.isExpired now)
