module Dunbar.Friend
( Friend,
  newFriend,
  _lastSeen,
  _name
)
where

import Dunbar.Data.Birthday (Birthday)
import Dunbar.Data.FullName (FullName, fullName)
import Data.DateTime (DateTime)
import Data.Maybe (Maybe(..))
import Utils.Lens (Lens', lens)

type Friend = {
  name :: FullName,
  dateOfBirth :: Maybe Birthday,
  yearOfBirth :: Maybe Int,
  lastSeen :: Maybe DateTime,
  notes :: String
}

_name :: Lens' Friend FullName
_name = lens _.name (_ {name = _})

_dateOfBirth :: Lens' Friend (Maybe Birthday)
_dateOfBirth = lens _.dateOfBirth (_ {dateOfBirth = _})

_yearOfBirth :: Lens' Friend (Maybe Int)
_yearOfBirth = lens _.yearOfBirth (_ {yearOfBirth = _})

_lastSeen :: Lens' Friend (Maybe DateTime)
_lastSeen = lens _.lastSeen (_ {lastSeen = _})

_notes :: Lens' Friend String
_notes = lens _.notes (_ {notes = _})

newFriend :: String -> String -> Friend
newFriend firstName lastName = {
  name: fullName firstName lastName,
  dateOfBirth: Nothing,
  yearOfBirth: Nothing,
  lastSeen: Nothing,
  notes: ""
}
