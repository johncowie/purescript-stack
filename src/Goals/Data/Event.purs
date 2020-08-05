module Goals.Data.Event where

import Prelude
import Utils.JsonDateTime (JsonDateTime)
import Utils.IdMap (Id)
import Data.Newtype (wrap)
import Data.DateTime (DateTime)
import Data.DateTime.Instant (Instant, toDateTime)
import Data.Either (Either(..))
import Record as Record
import Data.Argonaut.Core (stringify)
import Data.Argonaut.Decode (class DecodeJson, decodeJson, (.:))
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Symbol (SProxy(..))
import Data.Maybe (Maybe(..))
import Data.String as Str

data Event
  = AddGoal { title :: String, start :: JsonDateTime, end :: JsonDateTime, target :: Int }
  | AddTodo { name :: String, due :: JsonDateTime, comments :: String }
  | AddProgress { id :: Id, time :: JsonDateTime, amount :: Number, comment :: Maybe String }
  | RestartGoal { title :: String, start :: JsonDateTime, end :: JsonDateTime, target :: Int, predecessor :: Id }
  | UndoEvent { event :: Event }
  | CompletedTodo { id :: Id, completedAt :: JsonDateTime }

addGoalEvent :: String -> DateTime -> DateTime -> Int -> Event
addGoalEvent title start end target =
  AddGoal
    { title: title
    , start: wrap start
    , end: wrap end
    , target: target
    }

addTodoEvent :: String -> DateTime -> String -> Event
addTodoEvent name due comments = AddTodo { name, due: wrap due, comments }

completedTodoEvent :: Id -> Instant -> Event
completedTodoEvent id time = CompletedTodo { id, completedAt: wrap (toDateTime time) }

restartGoalEvent :: Id -> String -> DateTime -> DateTime -> Int -> Event
restartGoalEvent predecessor title start end target =
  RestartGoal
    { title: title
    , start: wrap start
    , end: wrap end
    , target: target
    , predecessor: predecessor
    }

addProgressEvent :: Id -> Instant -> Number -> String -> Event
addProgressEvent id time amount comment =
  AddProgress
    { id: id
    , time: wrap (toDateTime time)
    , amount: amount
    , comment: commentM
    }
  where
  commentM = case Str.trim comment of
    "" -> Nothing
    or -> Just comment

undoEvent :: Event -> Event
undoEvent event = UndoEvent { event }

type_ = SProxy :: SProxy "type"

instance showEvent :: Show Event where
  show = stringify <<< encodeJson

instance decodeJsonEvent :: DecodeJson Event where
  decodeJson json = do
    obj <- decodeJson json
    eventType <- obj .: "type"
    case eventType of
      "addGoal" -> AddGoal <$> decodeJson json
      "addProgress" -> AddProgress <$> decodeJson json
      "restartGoal" -> RestartGoal <$> decodeJson json
      "undoEvent" -> UndoEvent <$> decodeJson json
      "addTodo" -> AddTodo <$> decodeJson json
      "completedTodo" -> CompletedTodo <$> decodeJson json
      other -> (Left "Uknown event type")

instance encodeJsonEvent :: EncodeJson Event where
  encodeJson (AddGoal r) = encodeJson (Record.insert type_ "addGoal" r)
  encodeJson (AddProgress r) = encodeJson (Record.insert type_ "addProgress" r)
  encodeJson (RestartGoal r) = encodeJson (Record.insert type_ "restartGoal" r)
  encodeJson (UndoEvent r) = encodeJson (Record.insert type_ "undoEvent" r)
  encodeJson (AddTodo r) = encodeJson (Record.insert type_ "addTodo" r)
  encodeJson (CompletedTodo r) = encodeJson (Record.insert type_ "completedTodo" r)
