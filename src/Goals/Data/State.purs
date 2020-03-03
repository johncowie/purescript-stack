module Goals.Data.State where

import Prelude
import Data.Newtype (unwrap)
import Data.DateTime (DateTime)
import Data.Lens as L
import Utils.IdMap as IdMap
import Utils.JsonDateTime (JsonDateTime)
import Goals.Data.Goal (Goal, newUnitGoal)
import Goals.Data.Goal as Goal
import Goals.Data.Event (Event(..))

type GoalState = IdMap.IdMap Goal

newGoalState :: GoalState
newGoalState = IdMap.new

goalFromEventRecord :: {title :: String, start :: JsonDateTime, end :: JsonDateTime, target :: Int} -> Goal
goalFromEventRecord r = newUnitGoal r.title (unwrap r.start) (unwrap r.end) r.target

addProgressToGoal :: DateTime -> Int -> Goal -> Goal
addProgressToGoal time amount goal = L.over Goal.amountDoneL ((+) amountToAdd) goal
  where start = L.view Goal.startL goal
        end = L.view Goal.endL goal
        isWithinTimePeriod = (start <= time) && (time <= end)
        amountToAdd = if isWithinTimePeriod then amount else 0

addProgress :: IdMap.Id -> DateTime -> Int -> GoalState -> GoalState
addProgress id time amount = IdMap.update id (addProgressToGoal time amount)

processEvent :: Event -> GoalState -> GoalState
processEvent (AddGoal r) = IdMap.add (goalFromEventRecord r)
processEvent (AddProgress r) = addProgress r.id (unwrap r.time) r.amount
