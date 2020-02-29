module Dunbar.Friend
( Friend,
  newFriend,
  lastSeenL,
  nameL
)
where

import Dunbar.Utils.DateTime (DateTime)
import Dunbar.Utils.Birthday (Birthday)
import Dunbar.Utils.FullName (FullName, fullName)
import Data.Maybe (Maybe(..))
import Data.Lens (Lens', lens)

type Friend = {
  name :: FullName,
  dateOfBirth :: Maybe Birthday,
  yearOfBirth :: Maybe Int,
  lastSeen :: Maybe DateTime,
  notes :: String
}

nameL :: Lens' Friend FullName
nameL = lens _.name (_ {name = _})

dateOfBirthL :: Lens' Friend (Maybe Birthday)
dateOfBirthL = lens _.dateOfBirth (_ {dateOfBirth = _})

yearOfBirthL :: Lens' Friend (Maybe Int)
yearOfBirthL = lens _.yearOfBirth (_ {yearOfBirth = _})

lastSeenL :: Lens' Friend (Maybe DateTime)
lastSeenL = lens _.lastSeen (_ {lastSeen = _})

notesL :: Lens' Friend String
notesL = lens _.notes (_ {notes = _})

newFriend :: String -> String -> Friend
newFriend firstName lastName = {
  name: fullName firstName lastName,
  dateOfBirth: Nothing,
  yearOfBirth: Nothing,
  lastSeen: Nothing,
  notes: ""
}
