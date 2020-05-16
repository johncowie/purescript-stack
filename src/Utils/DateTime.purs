module Utils.DateTime where

import Prelude

import Data.Enum (class BoundedEnum, toEnum)
import Data.Date (canonicalDate, Date)
import Data.Formatter.DateTime as F
import Data.DateTime (DateTime(..), diff, adjust)
import Data.DateTime.Instant (Instant, fromDateTime, toDateTime, unInstant, instant)
import Data.Newtype (unwrap, wrap)
import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..), maybe, fromMaybe)
import Data.Time.Duration (Milliseconds, Seconds, Days)
import Data.Int (round, toNumber)
import Effect.Exception.Unsafe (unsafeThrow)

-- TODO fix me
toEnumUnsafe :: forall a. (BoundedEnum a) => Int -> a
toEnumUnsafe i = case toEnum i of
  Nothing -> unsafeThrow ("ARGHHH invalid date value: " <> show i)
  Just a -> a

dateToDateTime :: Date -> DateTime
dateToDateTime d = DateTime d bottom

showDate :: Date -> String
showDate = either unsafeThrow identity <<< F.formatDateTime "DD/MM/YYYY" <<< dateToDateTime

showDayMonth :: DateTime -> String
showDayMonth = either unsafeThrow identity <<< F.formatDateTime "DD/MM"

dateTimeToMillis :: DateTime -> Number
dateTimeToMillis = unwrap <<< unInstant <<< fromDateTime

millisToDateTime :: Number -> Either String DateTime
millisToDateTime millis = maybe (Left "Timestamp not valid") Right (toDateTime <$> (instant $ wrap millis))

newDate :: Int -> Int -> Int -> Date
newDate y m d = canonicalDate (toEnumUnsafe y) (toEnumUnsafe m) (toEnumUnsafe d)

diffMillis :: Instant -> Instant -> Milliseconds
diffMillis a b = wrap (unwrap (unInstant a) - unwrap (unInstant b))

diffSecs :: Instant -> Instant -> Seconds
diffSecs a b = wrap $ (\n -> n / 1000.0) $ unwrap $ diffMillis a b

daysToInt :: Days -> Int
daysToInt = unwrap >>> round

timeElapsedStr :: Seconds -> String
timeElapsedStr secs = case round $ unwrap secs of
  s | s < secondsLimit -> show s <> " seconds"
  s | s < minutesLimit -> show (mins s) <> " minutes"
  s | s < hoursLimit -> show (hours s) <> " hours"
  s -> "Some time has elapsed"
  where secondsLimit = 120
        minutesLimit = minSecs * 120
        hoursLimit = hourSecs * 24
        daysHoursLimit = daySecs * 3
        mins s = round (toNumber s / toNumber minSecs)
        hours s = round (toNumber s / toNumber hourSecs)
        days s = round (toNumber s / toNumber daySecs)
        minSecs = 60
        hourSecs = minSecs * 60
        daySecs = hourSecs * 24

nextDateTime :: DateTime -> DateTime -> DateTime
nextDateTime start end = fromMaybe end $ adjust d end
  where (d :: Seconds) = diff end start

-- newDateTime :: Int -> Int -> Int -> DateTime
-- newDateTime y m d = DateTime (newDate y m d) midnight
