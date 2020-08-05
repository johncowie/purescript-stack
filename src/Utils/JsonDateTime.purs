module Utils.JsonDateTime where

import Prelude
import Data.DateTime as DT
import Data.DateTime.Instant (Instant)
import Data.DateTime.Instant as DI
import Data.Time.Duration (Days)
import Data.Argonaut.Core (jsonNull)
import Data.Argonaut.Decode (class DecodeJson, decodeJson)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Formatter.DateTime as F
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Either (either)

newtype JsonDateTime
  = JsonDateTime DT.DateTime

derive instance newtypeJsonDateTime :: Newtype JsonDateTime _

dateStrFormat :: String
dateStrFormat = "YYYY/MM/DD HH:mm:ss.SSS"

instance decodeJsonDateTime :: DecodeJson JsonDateTime where
  decodeJson json = do
    dateStr <- decodeJson json
    date <- F.unformatDateTime dateStrFormat dateStr
    pure $ wrap date

instance encodeJsonDateTime :: EncodeJson JsonDateTime where
  encodeJson jdt = either (const jsonNull) encodeJson dateStrE
    where
    dateStrE = F.formatDateTime dateStrFormat (unwrap jdt)

---
newtype JsonInstant
  = JsonInstant Instant

derive instance newtypeJsonInstant :: Newtype JsonInstant _

toDateTime :: JsonInstant -> JsonDateTime
toDateTime = unwrap >>> DI.toDateTime >>> wrap

fromDateTime :: JsonDateTime -> JsonInstant
fromDateTime = unwrap >>> DI.fromDateTime >>> wrap

instance decodeJsonInstant :: DecodeJson JsonInstant where
  decodeJson json = fromDateTime <$> decodeJson json

instance encodeJsonInstant :: EncodeJson JsonInstant where
  encodeJson = toDateTime >>> encodeJson

newtype JsonDays
  = JsonDays Days

derive instance newtypeJsonDays :: Newtype JsonDays _

instance decodeJsonDays :: DecodeJson JsonDays where
  decodeJson json = wrap <$> wrap <$> decodeJson json

instance encodeJsonDays :: EncodeJson JsonDays where
  encodeJson jsonDays = encodeJson $ unwrap $ unwrap jsonDays
