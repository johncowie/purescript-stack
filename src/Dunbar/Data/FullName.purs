module Dunbar.Data.FullName where

import Prelude
import Utils.Lens as L
import Data.Newtype (class Newtype, unwrap)

data FullName = FullName {
  firstName :: String,
  lastName :: String
}

fullName :: String -> String -> FullName
fullName firstName lastName = FullName {firstName: firstName, lastName: lastName}

_firstName :: L.Lens' FullName String
_firstName = L._newtype >>> L.lens _.firstName (_ {firstName = _})

_lastName :: L.Lens' FullName String
_lastName = L._newtype >>> L.lens _.lastName (_ {lastName = _})

instance newTypeFullName :: Newtype FullName {firstName :: String, lastName :: String} where
  wrap = FullName
  unwrap (FullName r) = r

instance showFullName :: Show FullName where
  show (FullName {firstName, lastName}) = firstName <> " " <> lastName

instance ordFullName :: Ord FullName where
  compare a b = compare (show a) (show b)

instance eqFullName :: Eq FullName where
  eq a b = eq (unwrap a) (unwrap b)
