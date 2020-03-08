module Goals.Data.Event where

import Utils.JsonDateTime (JsonDateTime)
import Utils.IdMap (Id)
import Data.Newtype (wrap)
import Data.DateTime (DateTime)
import Data.DateTime.Instant (Instant, toDateTime)

data Event =
  AddGoal { title :: String, start :: JsonDateTime, end :: JsonDateTime, target :: Int} |
  AddProgress { id :: Id, time :: JsonDateTime, amount :: Int}

addGoalEvent :: String -> DateTime -> DateTime -> Int -> Event
addGoalEvent title start end target = AddGoal { title: title,
                                                start: wrap start,
                                                end: wrap end,
                                                target: target}

addProgressEvent :: Id -> Instant -> Int -> Event
addProgressEvent id time amount = AddProgress { id: id,
                                                time: wrap (toDateTime time),
                                                amount: amount}
