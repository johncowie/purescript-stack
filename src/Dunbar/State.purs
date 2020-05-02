module Dunbar.State where

import Prelude
import Dunbar.Friend (Friend, newFriend, _lastSeen)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple)
import Utils.Lens as L
import Data.Symbol (SProxy(..))
import Data.Newtype (unwrap)
import Data.Argonaut.Decode (class DecodeJson, decodeJson, (.:))
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Either (Either(..))
import Record as Record
import Utils.JsonDateTime (JsonDateTime)
import Utils.IdMap as IdMap

type Friendships = IdMap.IdMap Friend

data Event =
  AddFriend {firstName :: String, lastName :: String}
  | JustSeen {id :: IdMap.Id, timeSeen :: JsonDateTime}
  | DeleteFriend {id :: IdMap.Id}

addFriendEvent :: String -> String -> Event
addFriendEvent firstName lastName = AddFriend {firstName, lastName}

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
      other -> (Left "Uknown event type")

instance encodeJsonEvent :: EncodeJson Event where
  encodeJson (AddFriend r) = encodeJson (Record.insert type_ "addFriend" r)
  encodeJson (JustSeen r) = encodeJson (Record.insert type_ "justSeen" r)
  encodeJson (DeleteFriend r) = encodeJson (Record.insert type_ "deleteFriend" r)

empty :: Friendships
empty = IdMap.new

processEvent :: Event -> Friendships -> Friendships
processEvent (AddFriend r) = IdMap.add (newFriend r.firstName r.lastName)
processEvent (JustSeen r) = IdMap.update r.id (L.set _lastSeen (Just (unwrap r.timeSeen)))
processEvent (DeleteFriend r) = IdMap.delete r.id

friendList :: Friendships -> Array (Tuple IdMap.Id Friend)
friendList = IdMap.toArray

-- playEvents :: Array StateEvent -> Friendships

-- applyEvent :: StateEvent -> Friendships -> Friendships
