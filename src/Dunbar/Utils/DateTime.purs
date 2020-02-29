module Dunbar.Utils.DateTime (
  DateTime,
  dateTimeToMillis,
  formatDateTime,
  unformatDateTime,
  instantToDateTime
)
where

import Prelude

import Data.DateTime as DT
import Data.Argonaut.Core (jsonNull)
import Data.Argonaut.Decode (class DecodeJson, decodeJson)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.DateTime.Instant (fromDateTime, toDateTime, unInstant, instant, Instant)
import Data.Formatter.DateTime as F
import Data.Newtype (unwrap)
import Data.Either (Either(..), either)
import Data.Maybe (maybe)
import Data.Time.Duration (Milliseconds(..))

newtype DateTime = DateTime DT.DateTime

unwrapDateTime :: DateTime -> DT.DateTime
unwrapDateTime (DateTime dt) = dt

wrapDateTime :: DT.DateTime -> DateTime
wrapDateTime dt = DateTime dt

dateTimeToMillis :: DateTime -> Number
dateTimeToMillis = unwrap <<< unInstant <<< fromDateTime <<< unwrapDateTime

millisToDateTime :: Number -> Either String DateTime
millisToDateTime = (<$>) (wrapDateTime <<< toDateTime)
                   <<< (maybe (Left "Timestamp not valid") Right)
                   <<< instant
                   <<< Milliseconds

dateStrFormat :: String
dateStrFormat = "YYYY/MM/DD HH:mm:ss.SSS"

instance decodeJsonDateTime :: DecodeJson DateTime where
  decodeJson json = do
    dateStr <- decodeJson json
    date <- F.unformatDateTime dateStrFormat dateStr
    pure $ wrapDateTime date

instance encodeJsonDateTime :: EncodeJson DateTime where
  encodeJson (DateTime dt) = either (const jsonNull) encodeJson dateStrE
    where dateStrE = F.formatDateTime dateStrFormat dt

formatDateTime :: String -> DateTime -> Either String String
formatDateTime format = F.formatDateTime format <<< unwrapDateTime

unformatDateTime :: String -> String -> Either String DateTime
unformatDateTime format s = wrapDateTime <$> F.unformatDateTime format s

instantToDateTime :: Instant -> DateTime
instantToDateTime = wrapDateTime <<< toDateTime
