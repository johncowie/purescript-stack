module Utils.JsonNewtype where

import Prelude
import Data.Either (Either)
import Data.Newtype(class Newtype, wrap, unwrap)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (class DecodeJson, decodeJson)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)

decodeNewtype :: forall w u. (DecodeJson u) => (Newtype w u) => Json -> Either String w
decodeNewtype json = wrap <$> decodeJson json

encodeNewtype :: forall w u. (EncodeJson u) => (Newtype w u) => w -> Json
encodeNewtype = unwrap >>> encodeJson
