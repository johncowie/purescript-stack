module Dunbar.State where

import Prelude
import Dunbar.Friend (Friend, newFriend, lastSeenL)
import Dunbar.Utils.DateTime (DateTime)
import Data.Map as M
import Data.Map (Map)
import Data.Foldable (maximum)
import Data.Maybe (fromMaybe, Maybe(..))
import Data.Tuple (Tuple)
import Data.Lens as L
import Data.Argonaut.Core (jsonEmptyObject)
import Data.Argonaut.Decode (class DecodeJson, decodeJson, (.?))
import Data.Argonaut.Encode (class EncodeJson, (:=), (~>))
import Data.Either (Either(..))

type Friendships = Map Int Friend

data StateEvent =
  AddFriend String String
  | JustSeen {id :: Int, timeSeen :: DateTime}
  | DeleteFriend Int

instance decodeJsonEvent :: DecodeJson StateEvent where
  decodeJson json = do
    obj <- decodeJson json
    eventType <- obj .? "type"
    case eventType of
      "addFriend" -> do
        firstName <- obj .? "firstName"
        lastName <- obj .? "lastName"
        pure $ AddFriend firstName lastName
      --"justSeen" -> do
      other -> (Left "Uknown event type")

instance encodeJsonEvent :: EncodeJson StateEvent where
  encodeJson (AddFriend firstName lastName) = do
    "firstName" := firstName
    ~> "lastName" := lastName
    ~> "type" := "addFriend"
    ~> jsonEmptyObject
  encodeJson (JustSeen r) = do
    "id" := r.id
    ~> "timeSeen" := r.timeSeen
    ~> "type" := "justSeen"
    ~> jsonEmptyObject
  encodeJson (DeleteFriend id) = do
    "id" := id
    ~> "type" := "deleteFriend"
    ~> jsonEmptyObject

newFriendList :: Friendships
newFriendList = M.empty

createId :: Friendships -> Int
createId = (+) 1 <<< fromMaybe (-1) <<< maximum <<< M.keys

addFriend :: Friend -> Friendships -> Friendships
addFriend f m = M.insert (createId m) f m

updateFriend :: Int -> (Friend -> Friend) -> Friendships -> Friendships
updateFriend id fn = M.update (Just <<< fn) id

deleteFriend :: Int -> Friendships -> Friendships
deleteFriend id = M.update (const Nothing) id

updateState :: StateEvent -> Friendships -> Friendships
updateState (AddFriend fn ln) = addFriend (newFriend fn ln)
updateState (JustSeen r) = updateFriend r.id (L.set lastSeenL (Just r.timeSeen))
updateState (DeleteFriend id) = deleteFriend id

friendList :: Friendships -> Array (Tuple Int Friend)
friendList = M.toUnfoldable

-- playEvents :: Array StateEvent -> Friendships

-- applyEvent :: StateEvent -> Friendships -> Friendships
