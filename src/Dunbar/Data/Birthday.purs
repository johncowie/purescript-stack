module Dunbar.Data.Birthday
(Birthday, birthday, monthL, dayL)
where

import Data.Lens (Lens', lens)

type Birthday = {
  month :: Int,
  day :: Int
}

monthL :: Lens' Birthday Int
monthL = lens _.month (_ {month = _})

dayL :: Lens' Birthday Int
dayL = lens _.day (_ {day = _})

birthday :: Int -> Int -> Birthday
birthday month day = {month: month, day: day}
