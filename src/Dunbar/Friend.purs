module Dunbar.Friend
( Friend,
  newFriend,
  timeSinceLastSeen,
  overdueContact,
  _lastSeen,
  _name,
  _desiredContactFrequency
)
where

import Prelude
import Dunbar.Data.Birthday (Birthday)
import Dunbar.Data.FullName (FullName, fullName)
import Data.DateTime.Instant (Instant)
import Data.Time.Duration (Seconds, Days, convertDuration)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Data.Newtype (unwrap, wrap)
import Utils.Lens as L
import Utils.DateTime as UDT
-- import Effect.Exception.Unsafe (unsafeThrow)

type Friend = {
  name :: FullName,
  dateOfBirth :: Maybe Birthday,
  yearOfBirth :: Maybe Int,
  lastSeen :: Maybe Instant,
  desiredContactFrequency :: Maybe Days,
  notes :: Maybe String
}

_name :: L.Lens' Friend FullName
_name = L.prop (SProxy :: SProxy "name")

_dateOfBirth :: L.Lens' Friend (Maybe Birthday)
_dateOfBirth = L.prop (SProxy :: SProxy "dateOfBirth")

_yearOfBirth :: L.Lens' Friend (Maybe Int)
_yearOfBirth = L.prop (SProxy :: SProxy "yearOfBirth")

_lastSeen :: L.Lens' Friend (Maybe Instant)
_lastSeen = L.prop (SProxy :: SProxy "lastSeen")

_desiredContactFrequency :: L.Lens' Friend (Maybe Days)
_desiredContactFrequency = L.prop (SProxy :: SProxy "desiredContactFrequency")

_notes :: L.Lens' Friend (Maybe String)
_notes = L.prop (SProxy :: SProxy "notes")

newFriend :: String -> String -> Friend
newFriend firstName lastName = {
  name: fullName firstName lastName,
  dateOfBirth: Nothing,
  yearOfBirth: Nothing,
  lastSeen: Nothing,
  desiredContactFrequency: Nothing,
  notes: Nothing
}

timeSinceLastSeen :: Instant -> Friend -> Maybe Seconds
timeSinceLastSeen i f = (UDT.diffSecs i) <$> L.view _lastSeen f

overdueContact :: Instant -> Friend -> Maybe Seconds
overdueContact i f = do
  sinceSeenSecs <- timeSinceLastSeen i f
  (freqSecs :: Seconds) <- convertDuration <$> f.desiredContactFrequency
  pure $ wrap $ max 0.0 (unwrap sinceSeenSecs - unwrap freqSecs)
