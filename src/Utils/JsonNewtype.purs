module Utils.JsonNewtype where

import Prelude
import Data.Either (Either(..))
import Data.Newtype(class Newtype, wrap, unwrap)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (class DecodeJson, decodeJson)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)

decodeNewtype :: forall w u. (DecodeJson u) => String -> (Newtype w u) => Json -> Either String w
decodeNewtype newtypeName json = case decodeJson json of
  (Left err) -> Left $ newtypeName <> ": " <> err
  (Right val) -> Right $ wrap val

encodeNewtype :: forall w u. (EncodeJson u) => (Newtype w u) => w -> Json
encodeNewtype = unwrap >>> encodeJson
