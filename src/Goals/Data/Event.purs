module Goals.Data.Event where

import Utils.JsonDateTime (JsonDateTime)
import Utils.IdMap (Id)
import Data.Newtype (wrap)
import Data.DateTime (DateTime)

data Event =
  AddGoal { title :: String, start :: JsonDateTime, end :: JsonDateTime, target :: Int} |
  AddProgress { id :: Id, time :: JsonDateTime, amount :: Int}

addGoalEvent :: String -> DateTime -> DateTime -> Int -> Event
addGoalEvent title start end target = AddGoal { title: title,
                                                start: wrap start,
                                                end: wrap end,
                                                target: target
                                              }
