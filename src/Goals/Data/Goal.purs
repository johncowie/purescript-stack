module Goals.Data.Goal
( _start
, _end
, _title
, _target
, _amountDone
, _predecessor
, Goal
, isCurrent
, isExpired
, newUnitGoal
)
where

import Prelude
import Data.DateTime (DateTime)
import Utils.Lens (Lens', _newtype, prop)
import Data.Symbol (SProxy(..))
import Data.Newtype (class Newtype)
import Data.Maybe (Maybe(..))
import Utils.IdMap as IdMap
-- import Effect.Exception.Unsafe (unsafeThrow)

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

isCurrent :: DateTime -> Goal -> Boolean
isCurrent now (Goal r) = r.start <= now && r.end >= now

isExpired :: DateTime -> Goal -> Boolean
isExpired now (Goal r) = r.end < now
