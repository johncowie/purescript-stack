module Dunbar.State where

import Prelude
import Dunbar.Friend (Friend, newFriend, lastSeenL)
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

data StateEvent =
  AddFriend {firstName :: String, lastName :: String}
  | JustSeen {id :: IdMap.Id, timeSeen :: JsonDateTime}
  | DeleteFriend {id :: IdMap.Id}

type_ = SProxy :: SProxy "type"

dateStrFormat :: String
dateStrFormat = "YYYY/MM/DD HH:mm:ss.SSS"

instance decodeJsonEvent :: DecodeJson StateEvent where
  decodeJson json = do
    obj <- decodeJson json
    eventType <- obj .: "type"
    case eventType of
      "addFriend" -> AddFriend <$> decodeJson json
      "justSeen" -> JustSeen <$> decodeJson json
      "deleteFriend" -> DeleteFriend <$> decodeJson json
      other -> (Left "Uknown event type")

instance encodeJsonEvent :: EncodeJson StateEvent where
  encodeJson (AddFriend r) = encodeJson (Record.insert type_ "addFriend" r)
  encodeJson (JustSeen r) = encodeJson (Record.insert type_ "justSeen" r)
  encodeJson (DeleteFriend r) = encodeJson (Record.insert type_ "deleteFriend" r)

newFriendList :: Friendships
newFriendList = IdMap.new

updateState :: StateEvent -> Friendships -> Friendships
updateState (AddFriend r) = IdMap.add (newFriend r.firstName r.lastName)
updateState (JustSeen r) = IdMap.update r.id (L.set lastSeenL (Just (unwrap r.timeSeen)))
updateState (DeleteFriend r) = IdMap.delete r.id

friendList :: Friendships -> Array (Tuple IdMap.Id Friend)
friendList = IdMap.toArray

-- playEvents :: Array StateEvent -> Friendships

-- applyEvent :: StateEvent -> Friendships -> Friendships
