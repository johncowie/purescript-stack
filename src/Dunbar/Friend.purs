module Dunbar.Friend
( Friend
, newFriend
, timeSinceLastSeen
, overdueContact
, _lastSeen
, _name
, _birthday
, _desiredContactFrequency
, _notes
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
import Utils.JsonDateTime (JsonInstant, JsonDays)
-- import Effect.Exception.Unsafe (unsafeThrow)

type Friend = {
  name :: FullName,
  birthday :: Maybe Birthday,
  lastSeen :: Maybe JsonInstant,
  desiredContactFrequency :: Maybe JsonDays,
  notes :: Maybe String
}

_name :: L.Lens' Friend FullName
_name = L.prop (SProxy :: SProxy "name")

_birthday :: L.Lens' Friend (Maybe Birthday)
_birthday = L.prop (SProxy :: SProxy "birthday")

_lastSeen :: L.Lens' Friend (Maybe Instant)
_lastSeen = L.prop (SProxy :: SProxy "lastSeen") >>> L.liftLens L._newtype

_desiredContactFrequency :: L.Lens' Friend (Maybe Days)
_desiredContactFrequency = L.prop (SProxy :: SProxy "desiredContactFrequency") >>> L.liftLens L._newtype

_notes :: L.Lens' Friend (Maybe String)
_notes = L.prop (SProxy :: SProxy "notes")

newFriend :: String -> String -> Friend
newFriend firstName lastName = {
  name: fullName firstName lastName,
  birthday: Nothing,
  lastSeen: Nothing,
  desiredContactFrequency: Nothing,
  notes: Nothing
}

timeSinceLastSeen :: Instant -> Friend -> Maybe Seconds
timeSinceLastSeen i f = (UDT.diffSecs i) <$> L.view _lastSeen f

overdueContact :: Instant -> Friend -> Maybe Seconds
overdueContact i f = do
  sinceSeenSecs <- timeSinceLastSeen i f
  (freqSecs :: Seconds) <- convertDuration <$> unwrap <$> f.desiredContactFrequency
  pure $ wrap $ max 0.0 (unwrap sinceSeenSecs - unwrap freqSecs)
