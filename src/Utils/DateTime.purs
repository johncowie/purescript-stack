module Utils.DateTime where

import Prelude

import Data.Enum (class BoundedEnum, toEnum)
import Data.Date (canonicalDate, Date)
import Data.DateTime (DateTime)
import Data.Formatter.DateTime as F
import Data.DateTime.Instant (fromDateTime, toDateTime, unInstant, instant)
import Data.Newtype (unwrap)
import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..), maybe)
import Data.Time.Duration (Milliseconds(..))
import Effect.Exception.Unsafe (unsafeThrow)

-- TODO fix me
toEnumUnsafe :: forall a. (BoundedEnum a) => Int -> a
toEnumUnsafe i = case toEnum i of
  Nothing -> unsafeThrow ("ARGHHH invalid date value: " <> show i)
  Just a -> a

parseDate :: String -> Either String DateTime
parseDate = F.unformatDateTime "DD/MM/YYYY"

parseDateUnsafe :: String -> DateTime
parseDateUnsafe = either unsafeThrow identity <<< parseDate

dateTimeToMillis :: DateTime -> Number
dateTimeToMillis = unwrap <<< unInstant <<< fromDateTime

millisToDateTime :: Number -> Either String DateTime
millisToDateTime millis = maybe (Left "Timestamp not valid") Right (toDateTime <$> (instant $ Milliseconds millis))

newDate :: Int -> Int -> Int -> Date
newDate y m d = canonicalDate (toEnumUnsafe y) (toEnumUnsafe m) (toEnumUnsafe d)

-- newDateTime :: Int -> Int -> Int -> DateTime
-- newDateTime y m d = DateTime (newDate y m d) midnight
