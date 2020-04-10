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
import Data.Maybe (Maybe(..))
import Data.String as Str

data Event =
  AddGoal { title :: String, start :: JsonDateTime, end :: JsonDateTime, target :: Int} |
  AddProgress {id :: Id, time :: JsonDateTime, amount :: Number, comment :: Maybe String } |
  RestartGoal { title :: String, start :: JsonDateTime, end :: JsonDateTime, target :: Int, predecessor :: Id}

addGoalEvent :: String -> DateTime -> DateTime -> Int -> Event
addGoalEvent title start end target = AddGoal { title: title,
                                                start: wrap start,
                                                end: wrap end,
                                                target: target}

restartGoalEvent :: Id -> String -> DateTime -> DateTime -> Int -> Event
restartGoalEvent predecessor title start end target =
  RestartGoal { title: title,
                start: wrap start,
                end: wrap end,
                target: target,
                predecessor: predecessor}

addProgressEventV2 :: Id -> Instant -> Number -> String -> Event
addProgressEventV2 id time amount comment =
  AddProgress { id: id,
                  time: wrap (toDateTime time),
                  amount: amount,
                  comment: commentM }
  where commentM = case Str.trim comment of
                    "" -> Nothing
                    or -> Just comment

type_ = SProxy :: SProxy "type"

instance decodeJsonEvent :: DecodeJson Event where
  decodeJson json = do
    obj <- decodeJson json
    eventType <- obj .: "type"
    case eventType of
      "addGoal" -> AddGoal <$> decodeJson json
      "addProgress" -> AddProgress <$> decodeJson json
      "restartGoal" -> RestartGoal <$> decodeJson json
      other -> (Left "Uknown event type")

instance encodeJsonEvent :: EncodeJson Event where
  encodeJson (AddGoal r) = encodeJson (Record.insert type_ "addGoal" r)
  encodeJson (AddProgress r) = encodeJson (Record.insert type_ "addProgress" r)
  encodeJson (RestartGoal r) = encodeJson (Record.insert type_ "restartGoal" r)
