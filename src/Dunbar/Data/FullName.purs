module Dunbar.Data.FullName where

import Prelude
import Utils.Lens (lens, Lens')

type FullName = {
  firstName :: String,
  lastName :: String
}

fullName :: String -> String -> FullName
fullName firstName lastName = {firstName: firstName, lastName: lastName}

firstNameL :: Lens' FullName String
firstNameL = lens _.firstName (_ {firstName = _})

lastNameL :: Lens' FullName String
lastNameL = lens _.lastName (_ {lastName = _})

showLastName :: FullName -> String
showLastName {firstName, lastName} = firstName <> " " <> lastName
