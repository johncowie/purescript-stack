module Dunbar.State
( Friendships
, Event
, addFriendEvent
, justSeenEvent
, updateDesiredContactFrequencyEvent
, updateNotesEvent
, updateBirthdayEvent
, processEvent
, friendList
, empty
)
where

import Prelude

import Dunbar.Friend (Friend)
import Dunbar.Friend as Friend
import Dunbar.Data.Birthday (Birthday)

import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple)
import Data.Symbol (SProxy(..))
import Data.Newtype (unwrap, wrap)
import Data.Argonaut.Decode (class DecodeJson, decodeJson, (.:))
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Either (Either(..))
import Data.DateTime.Instant (Instant, toDateTime, fromDateTime)
import Data.Int (toNumber)

import Record as Record

import Utils.Lens as L
import Utils.JsonDateTime (JsonDateTime)
import Utils.IdMap as IdMap

type Friendships = IdMap.IdMap Friend

data Event =
  AddFriend {firstName :: String, lastName :: String}
  | JustSeen {id :: IdMap.Id, timeSeen :: JsonDateTime}
  | DeleteFriend {id :: IdMap.Id}
  | UpdateDesiredContactFrequency {id :: IdMap.Id, desiredContactFrequencyDays :: Maybe Int}
  | UpdateNotes {id :: IdMap.Id, notes :: Maybe String}
  | UpdateBirthday {id :: IdMap.Id, birthday :: Maybe Birthday}

addFriendEvent :: String -> String -> Event
addFriendEvent firstName lastName = AddFriend {firstName, lastName}

justSeenEvent :: IdMap.Id -> Instant -> Event
justSeenEvent id instant = JustSeen {id, timeSeen: wrap (toDateTime instant)}

updateNotesEvent :: IdMap.Id -> (Maybe String) -> Event
updateNotesEvent id notes = UpdateNotes {id, notes}

updateDesiredContactFrequencyEvent :: IdMap.Id -> Maybe Int -> Event
updateDesiredContactFrequencyEvent id desiredContactFrequencyDays =
  UpdateDesiredContactFrequency {id, desiredContactFrequencyDays}

updateBirthdayEvent :: IdMap.Id -> Maybe Birthday -> Event
updateBirthdayEvent id birthday = UpdateBirthday {id, birthday}

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
      "updateNotes" -> UpdateNotes <$> decodeJson json
      "updateBirthday" -> UpdateBirthday <$> decodeJson json
      other -> (Left "Unknown event type")

instance encodeJsonEvent :: EncodeJson Event where
  encodeJson (AddFriend r) = encodeJson (Record.insert type_ "addFriend" r)
  encodeJson (JustSeen r) = encodeJson (Record.insert type_ "justSeen" r)
  encodeJson (DeleteFriend r) = encodeJson (Record.insert type_ "deleteFriend" r)
  encodeJson (UpdateDesiredContactFrequency r) = encodeJson (Record.insert type_ "updateDesiredContactFrequency" r)
  encodeJson (UpdateNotes r) = encodeJson (Record.insert type_ "updateNotes" r)
  encodeJson (UpdateBirthday r) = encodeJson (Record.insert type_ "updateBirthday" r)

empty :: Friendships
empty = IdMap.new

processEvent :: Event -> Friendships -> Friendships
processEvent (AddFriend r) = IdMap.add (Friend.newFriend r.firstName r.lastName)
processEvent (JustSeen r) = IdMap.update r.id (L.set Friend._lastSeen (Just $ fromDateTime $ unwrap $ r.timeSeen))
processEvent (DeleteFriend r) = IdMap.delete r.id
processEvent (UpdateDesiredContactFrequency r) = IdMap.update r.id (L.set Friend._desiredContactFrequency days)
  where days = wrap <$> toNumber <$> r.desiredContactFrequencyDays
processEvent (UpdateNotes r) = IdMap.update r.id (L.set Friend._notes r.notes)
processEvent (UpdateBirthday r) = IdMap.update r.id (L.set Friend._birthday r.birthday)

friendList :: Friendships -> Array (Tuple IdMap.Id Friend)
friendList = IdMap.toArray

-- playEvents :: Array StateEvent -> Friendships

-- applyEvent :: StateEvent -> Friendships -> Friendships
