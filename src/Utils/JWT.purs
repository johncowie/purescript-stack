module Utils.JWT
( JWT
, TokenGenerator
, JWTGenerator
, extractPayload
, jwtGenerator
, main
)
where

import CustomPrelude

import Data.Argonaut.Core as J
import Data.Argonaut.Decode (class DecodeJson, decodeJson)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Argonaut.Parser (jsonParser)
import Data.Either (Either(..))
import Data.Newtype (class Newtype, wrap, unwrap)
import Data.String as Str
import Data.Array ((!!))
import Data.Maybe (maybe)
import Data.String as Str
import Data.String.Regex as Re
import Data.String.Regex.Unsafe (unsafeRegex)
import Data.String.Regex.Flags as F
import Data.String.Base64 as Base64

import Effect (Effect)
import Effect.Console as Console

import Node.Crypto.Hmac as Hmac
import Node.Crypto.Hash (Algorithm(SHA256))

import Utils.ExceptT (showError)

newtype JWT = JWT String

derive instance newtypeJWT :: Newtype JWT _

instance decodeJsonJWT :: DecodeJson JWT where
  decodeJson = decodeJson >>> map wrap

instance encodeJsonJWT :: EncodeJson JWT where
  encodeJson = unwrap >>> encodeJson

type TokenGenerator token m payload = {
  generate :: payload -> m token
, verify :: token -> m Boolean
}

type JWTGenerator = TokenGenerator JWT Effect J.Json

jwtGenerator :: Hmac.Secret -> JWTGenerator
jwtGenerator secret = {
  generate: generateToken secret
, verify: verifyToken secret
}

base64ToBase64Url :: String -> String
base64ToBase64Url =
      Re.replace (unsafeRegex "\\=$" F.noFlags) ""
  >>> Str.replace (Str.Pattern "+") (Str.Replacement "-")
  >>> Str.replace (Str.Pattern "/") (Str.Replacement "_")

generateToken :: forall a. (EncodeJson a) => Hmac.Secret -> a -> Effect JWT
generateToken secret payload = do
  signature <- Hmac.base64 SHA256 secret (headerEncoded <> "." <> payloadEncoded)
  pure $ wrap $ headerEncoded <> "." <> payloadEncoded <> "." <> base64ToBase64Url signature
  where headerEncoded = Base64.encodeUrl $ J.stringify $ encodeJson {alg: "HS256", typ: "JWT"}
        payloadEncoded = Base64.encodeUrl $ J.stringify $ encodeJson payload

verifyToken :: Hmac.Secret -> JWT -> Effect Boolean
verifyToken secret jwt = pure false -- "FIXME implement me"

extractPayload :: forall a. (DecodeJson a) => JWT -> Either String a
extractPayload (JWT jwtStr) = do
  let parts = Str.split (Str.Pattern ".") jwtStr
  part <- maybe (Left "No second part of token") Right $ parts !! 1
  jsonStr <- showError $ Base64.decode part
  json <- jsonParser jsonStr
  decodeJson json

main :: Effect Unit
main = do
  token <- generateToken "secret" {fuck: "you dickwad"}
  Console.log (unwrap token)
