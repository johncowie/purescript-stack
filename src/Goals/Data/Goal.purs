module Goals.Data.Goal
( _start
, _end
, _title
, _target
, _amountDone
, _predecessor
, Goal
, isInProgress
, isExpired
, isFuture
, isCurrent
, newUnitGoal
, progressPercentage
, onTrackRequired
, timeElapsedPercentage
, requiredPercentage
)
where

import Prelude
import Data.DateTime (DateTime)
import Data.DateTime.Instant (Instant, fromDateTime, unInstant, toDateTime)
import Utils.Lens (Lens', _newtype, prop)
import Data.Symbol (SProxy(..))
import Data.Newtype (class Newtype, unwrap)
import Data.Maybe (Maybe(..))
import Data.Int as Int
import Utils.IdMap as IdMap
import Utils.Fixed as DF
import Effect.Exception.Unsafe (unsafeThrow)

newtype Goal = Goal {
  start :: DateTime,
  end :: DateTime,
  target :: Int,
  title :: String,
  amountDone :: Number,
  predecessor :: Maybe IdMap.Id
}

derive instance newtypeGoal :: Newtype Goal _

newUnitGoal :: String -> DateTime -> DateTime -> Int -> Goal
newUnitGoal title start end target =
  Goal {start: start,
        end: end,
        title: title,
        target: target,
        amountDone: 0.0,
        predecessor: Nothing
        }

_start :: Lens' Goal DateTime
_start = _newtype >>> prop (SProxy :: SProxy "start")

_end :: Lens' Goal DateTime
_end = _newtype >>> prop (SProxy :: SProxy "end")

_title :: Lens' Goal String
_title = _newtype >>> prop (SProxy :: SProxy "title")

_target :: Lens' Goal Int
_target = _newtype >>> prop (SProxy :: SProxy "target")

_amountDone :: Lens' Goal Number
_amountDone = _newtype >>> prop (SProxy :: SProxy "amountDone")

_predecessor :: Lens' Goal (Maybe IdMap.Id)
_predecessor = _newtype >>> prop (SProxy :: SProxy "predecessor")

isCurrent :: Instant -> Goal -> Boolean
isCurrent now (Goal r) = r.start <= nowDT && r.end >= nowDT
  where nowDT = toDateTime now

isInProgress :: Instant -> Goal -> Boolean
isInProgress now (Goal r) = r.start <= nowDT && r.end >= nowDT && r.amountDone < Int.toNumber r.target
  where nowDT = toDateTime now

isExpired :: Instant -> Goal -> Boolean
isExpired now (Goal r) = r.end < nowDT || r.amountDone >= Int.toNumber r.target
  where nowDT = toDateTime now

isFuture :: Instant -> Goal -> Boolean
isFuture now (Goal r) = r.start > nowDT
  where nowDT = toDateTime now


-- stats

progressPercentage :: Goal -> Number
progressPercentage (Goal goal) = min 100.0 $ (goal.amountDone / Int.toNumber goal.target) * 100.0

timeElapsedPercentage :: Instant -> Goal -> Number
timeElapsedPercentage now (Goal goal) = ((nowMillis - startMillis) / (endMillis - startMillis)) * 100.0
  where startMillis = instantMillis $ fromDateTime $ goal.start
        endMillis = instantMillis $ fromDateTime $ goal.end
        nowMillis = instantMillis now
        instantMillis instant = unwrap $ unInstant instant

onTrackRequired :: Instant -> Goal -> Number
onTrackRequired now (Goal goal) = requiredToDate - goal.amountDone
  where requiredToDate = (Int.toNumber goal.target * elapsedPC / 100.0)
        elapsedPC = timeElapsedPercentage now (Goal goal)

requiredPercentage :: Instant -> Goal -> Number
requiredPercentage now (Goal goal) = onTrackRequired now (Goal goal) / (Int.toNumber goal.target) * 100.0
