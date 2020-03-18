module Goals.Data.Goal where

import Data.DateTime (DateTime)
import Utils.Lens (Lens', lens)

type Goal = {
  start :: DateTime,
  end :: DateTime,
  target :: Int,
  title :: String,
  amountDone :: Int
}

newUnitGoal :: String -> DateTime -> DateTime -> Int -> Goal
newUnitGoal title start end target =
  {start: start,
   end: end,
   title: title,
   target: target,
   amountDone: 0}

startL :: Lens' Goal DateTime
startL = lens _.start (_ {start = _})

endL :: Lens' Goal DateTime
endL = lens _.end (_ {end = _})

titleL :: Lens' Goal String
titleL = lens _.title (_ {title = _})

targetL :: Lens' Goal Int
targetL = lens _.target (_ {target = _})

amountDoneL :: Lens' Goal Int
amountDoneL = lens _.amountDone (_ {amountDone = _})
