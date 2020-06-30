module Utils.JWT where

import Prelude
import Data.Argonaut.Decode (class DecodeJson, decodeJson)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Argonaut.Parser (jsonParser)
import Data.Either (Either(..))
import Data.Newtype (class Newtype, wrap, unwrap)
import Data.String as Str
import Data.Array ((!!))
import Data.Maybe (maybe)
import Data.String.Base64 as Base64

import Utils.ExceptT (showError)

newtype JWT = JWT String

derive instance newtypeJWT :: Newtype JWT _

instance decodeJsonJWT :: DecodeJson JWT where
  decodeJson = decodeJson >>> map wrap

instance encodeJsonJWT :: EncodeJson JWT where
  encodeJson = unwrap >>> encodeJson

extractPayload :: forall a. (DecodeJson a) => JWT -> Either String a
extractPayload (JWT jwtStr) = do
  let parts = Str.split (Str.Pattern ".") jwtStr
  part <- maybe (Left "No second part of token") Right $ parts !! 1
  jsonStr <- showError $ Base64.decode part
  json <- jsonParser jsonStr
  decodeJson json
