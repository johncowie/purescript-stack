module Dunbar.Data.Birthday
( Birthday
)
where


import Prelude

import Control.Alt ((<|>))

import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Date (Date, Day, Month, Year, year, month, day, exactDate)
import Data.DateTime as DT
import Data.Either (Either, hush)
import Data.Formatter.DateTime as F
import Data.Enum (toEnum)

import Effect.Exception.Unsafe (unsafeThrow)

import Utils.Lens (Lens')
import Utils.Lens as L
import Utils.Components.Input (class InputType)
import Utils.DateTime (dateToDateTime)

newtype Birthday = Birthday {
  month :: Month
, day :: Day
, year :: Maybe Year
}

derive instance newTypeBirthday :: Newtype Birthday _

instance birthdayInputType :: InputType Birthday where
  parseInput s = parseDayMonth s <|> parseDate s
  showInput b = case (L.view yearField b) of
    (Just _) -> showDate b
    Nothing -> showDayMonth b

toDate :: Birthday -> Maybe Date
toDate (Birthday b) = do
  year <- b.year <|> toEnum 2000
  exactDate year b.month b.day

-- use alt
parseDayMonth :: String -> Either String Birthday
parseDayMonth s = do
  datetime <- F.unformatDateTime "DD/MM" s
  let d = DT.date datetime
  pure $ Birthday {day: day d, month: month d, year: Nothing}

showDayMonth :: Birthday -> String
showDayMonth birthday = fromMaybe "01/01" $ do
  d <- toDate birthday
  hush $ F.formatDateTime "DD/MM" $ dateToDateTime d

parseDate :: String -> Either String Birthday
parseDate s = do
  datetime <- F.unformatDateTime "DD/MM/YYYY" s
  let d = DT.date datetime
  pure $ Birthday {day: day d, month: month d, year: Just (year d)}

showDate :: Birthday -> String
showDate birthday = fromMaybe "01/01/1970" $ do
  d <- toDate birthday
  hush $ F.formatDateTime "DD/MM/YYYY" $ dateToDateTime d

monthField :: Lens' Birthday Month
monthField = L.newtypeProp (SProxy :: SProxy "month")

dayField :: Lens' Birthday Day
dayField = L.newtypeProp (SProxy :: SProxy "day")

yearField :: Lens' Birthday (Maybe Year)
yearField = L.newtypeProp (SProxy :: SProxy "year")
