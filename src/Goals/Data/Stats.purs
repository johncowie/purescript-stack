module Goals.Data.Stats where

import Prelude
import Data.DateTime.Instant (Instant, fromDateTime, unInstant)
import Utils.Lens as L
import Data.Int (toNumber, round)
import Data.Newtype (unwrap)
import Goals.Data.State (GoalState)
import Goals.Data.Goal (Goal)
import Goals.Data.Goal as Goal
import Utils.IdMap (IdMap)
-- import Effect.Exception.Unsafe (unsafeThrow)

type GoalStats = {progressPercentage :: Number,
                  timeElapsedPercentage :: Number,
                  onTrackRequired :: Int}
type Stats = IdMap GoalStats

percentageTimeElapsed :: Instant -> Goal -> Number
percentageTimeElapsed now goal = ((nowMillis - startMillis) / (endMillis - startMillis)) * 100.0
  where startMillis = instantMillis $ fromDateTime $ L.view Goal._start goal
        endMillis = instantMillis $ fromDateTime $ L.view Goal._end goal
        nowMillis = instantMillis now
        instantMillis instant = unwrap $ unInstant instant

calculateOnTrackRequired :: Number -> Goal -> Int
calculateOnTrackRequired elapsedPC goal = round (requiredToDate - amountDone)
  where requiredToDate = (toNumber target * elapsedPC / 100.0)
        amountDone = L.view Goal._amountDone goal
        target = L.view Goal._target goal

goalStats :: Instant -> Goal -> GoalStats
goalStats now goal = {progressPercentage: progress,
                      onTrackRequired: calculateOnTrackRequired timeElapsedPercentage goal,
                      timeElapsedPercentage}
  where target = L.view Goal._target goal
        amountDone = L.view Goal._amountDone goal
        progress = min 100.0 $ (amountDone / toNumber target) * 100.0
        timeElapsedPercentage = percentageTimeElapsed now goal

calculateStats :: Instant -> GoalState -> Stats
calculateStats now = map (goalStats now)
