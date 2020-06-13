module Dunbar.Data.FullName where

import Prelude
import Utils.Lens as L
import Data.Newtype (class Newtype, unwrap)
import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Encode (class EncodeJson)
import Utils.JsonNewtype (encodeNewtype, decodeNewtype)

newtype FullName = FullName {
  firstName :: String,
  lastName :: String
}

fullName :: String -> String -> FullName
fullName firstName lastName = FullName {firstName: firstName, lastName: lastName}

_firstName :: L.Lens' FullName String
_firstName = L._newtype >>> L.lens _.firstName (_ {firstName = _})

_lastName :: L.Lens' FullName String
_lastName = L._newtype >>> L.lens _.lastName (_ {lastName = _})

derive instance newTypeFullName :: Newtype FullName _

instance showFullName :: Show FullName where
  show (FullName {firstName, lastName}) = firstName <> " " <> lastName

instance ordFullName :: Ord FullName where
  compare a b = compare (show a) (show b)

instance eqFullName :: Eq FullName where
  eq a b = eq (unwrap a) (unwrap b)

instance decodeJsonFullName :: DecodeJson FullName where
  decodeJson = decodeNewtype

instance encodeJsonFullName :: EncodeJson FullName where
  encodeJson = encodeNewtype
