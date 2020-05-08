module Dunbar.State
( Friendships
, Event
, addFriendEvent
, justSeenEvent
, updateDesiredContactFrequencyEvent
, processEvent
, friendList
, empty
)
where

import Prelude
import Dunbar.Friend (Friend)
import Dunbar.Friend as Friend
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple)
import Utils.Lens as L
import Data.Symbol (SProxy(..))
import Data.Newtype (unwrap, wrap)
import Data.Argonaut.Decode (class DecodeJson, decodeJson, (.:))
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Either (Either(..))
import Data.DateTime.Instant (Instant, toDateTime, fromDateTime)
import Data.Int (toNumber)
import Record as Record
import Utils.JsonDateTime (JsonDateTime)
import Utils.IdMap as IdMap

type Friendships = IdMap.IdMap Friend

data Event =
  AddFriend {firstName :: String, lastName :: String}
  | JustSeen {id :: IdMap.Id, timeSeen :: JsonDateTime}
  | DeleteFriend {id :: IdMap.Id}
  | UpdateDesiredContactFrequency {id :: IdMap.Id, desiredContactFrequencyDays :: Maybe Int}

addFriendEvent :: String -> String -> Event
addFriendEvent firstName lastName = AddFriend {firstName, lastName}

justSeenEvent :: IdMap.Id -> Instant -> Event
justSeenEvent id instant = JustSeen {id, timeSeen: wrap (toDateTime instant)}

updateDesiredContactFrequencyEvent :: IdMap.Id -> Maybe Int -> Event
updateDesiredContactFrequencyEvent id desiredContactFrequencyDays =
  UpdateDesiredContactFrequency {id, desiredContactFrequencyDays}

type_ = SProxy :: SProxy "type"

dateStrFormat :: String
dateStrFormat = "YYYY/MM/DD HH:mm:ss.SSS"

instance decodeJsonEvent :: DecodeJson Event where
  decodeJson json = do
    obj <- decodeJson json
    eventType <- obj .: "type"
    case eventType of
      "addFriend" -> AddFriend <$> decodeJson json
      "justSeen" -> JustSeen <$> decodeJson json
      "deleteFriend" -> DeleteFriend <$> decodeJson json
      "updateDesiredContactFrequency" -> UpdateDesiredContactFrequency <$> decodeJson json
      other -> (Left "Uknown event type")

instance encodeJsonEvent :: EncodeJson Event where
  encodeJson (AddFriend r) = encodeJson (Record.insert type_ "addFriend" r)
  encodeJson (JustSeen r) = encodeJson (Record.insert type_ "justSeen" r)
  encodeJson (DeleteFriend r) = encodeJson (Record.insert type_ "deleteFriend" r)
  encodeJson (UpdateDesiredContactFrequency r) = encodeJson (Record.insert type_ "updateDesiredContactFrequency" r)

empty :: Friendships
empty = IdMap.new

processEvent :: Event -> Friendships -> Friendships
processEvent (AddFriend r) = IdMap.add (Friend.newFriend r.firstName r.lastName)
processEvent (JustSeen r) = IdMap.update r.id (L.set Friend._lastSeen (Just $ fromDateTime $ unwrap $ r.timeSeen))
processEvent (DeleteFriend r) = IdMap.delete r.id
processEvent (UpdateDesiredContactFrequency r) = IdMap.update r.id (L.set Friend._desiredContactFrequency days)
  where days = wrap <$> toNumber <$> r.desiredContactFrequencyDays

friendList :: Friendships -> Array (Tuple IdMap.Id Friend)
friendList = IdMap.toArray

-- playEvents :: Array StateEvent -> Friendships

-- applyEvent :: StateEvent -> Friendships -> Friendships
