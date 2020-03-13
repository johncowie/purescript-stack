module Goals.Data.Stats where

import Prelude
import Data.DateTime.Instant (Instant)
import Data.Lens as L
import Data.Int (toNumber)
import Goals.Data.State (GoalState)
import Goals.Data.Goal (Goal)
import Goals.Data.Goal as Goal
import Utils.IdMap (IdMap)

type GoalStats = {progressPercentage :: Number}
type Stats = IdMap GoalStats

goalStats :: Instant -> Goal -> GoalStats
goalStats _now goal = {progressPercentage: progress}
  where target = L.view Goal.targetL goal
        amountDone = L.view Goal.amountDoneL goal
        progress = min 100.0 $ (toNumber amountDone / toNumber target) * 100.0

calculateStats :: Instant -> GoalState -> Stats
calculateStats now = map (goalStats now)
