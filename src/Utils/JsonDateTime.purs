module Utils.JsonDateTime where

import Prelude

import Data.DateTime as DT
import Data.Argonaut.Core (jsonNull)
import Data.Argonaut.Decode (class DecodeJson, decodeJson)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Formatter.DateTime as F
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Either (either)

newtype JsonDateTime = JsonDateTime DT.DateTime

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
    where dateStrE = F.formatDateTime dateStrFormat (unwrap jdt)
