module Goals.Data.Event where

import Prelude
import Utils.JsonDateTime (JsonDateTime)
import Utils.IdMap (Id)
import Data.Newtype (wrap)
import Data.DateTime (DateTime)
import Data.DateTime.Instant (Instant, toDateTime)
import Data.Either(Either(..))
import Record as Record
import Data.Argonaut.Decode (class DecodeJson, decodeJson, (.:))
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Symbol (SProxy(..))

data Event =
  AddGoal { title :: String, start :: JsonDateTime, end :: JsonDateTime, target :: Int} |
  AddProgress { id :: Id, time :: JsonDateTime, amount :: Number}

addGoalEvent :: String -> DateTime -> DateTime -> Int -> Event
addGoalEvent title start end target = AddGoal { title: title,
                                                start: wrap start,
                                                end: wrap end,
                                                target: target}

addProgressEvent :: Id -> Instant -> Number -> Event
addProgressEvent id time amount = AddProgress { id: id,
                                                time: wrap (toDateTime time),
                                                amount: amount}


type_ = SProxy :: SProxy "type"

instance decodeJsonEvent :: DecodeJson Event where
  decodeJson json = do
    obj <- decodeJson json
    eventType <- obj .: "type"
    case eventType of
      "addGoal" -> AddGoal <$> decodeJson json
      "addProgress" -> AddProgress <$> decodeJson json
      other -> (Left "Uknown event type")

instance encodeJsonEvent :: EncodeJson Event where
  encodeJson (AddGoal r) = encodeJson (Record.insert type_ "addGoal" r)
  encodeJson (AddProgress r) = encodeJson (Record.insert type_ "addProgress" r)
