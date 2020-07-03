module Server.Middleware.JSON
( JSONRequest
, JSONResponse
, okJsonResponse
, jsonResponse
, wrapJsonRequest
, wrapJsonResponse
)
where

import Prelude

import HTTPure as HP

import Data.Argonaut.Core (Json, stringify) as JSON
import Data.Argonaut.Encode (class EncodeJson, encodeJson) as JSON
import Data.Argonaut.Decode (class DecodeJson, decodeJson) as JSON
import Data.Argonaut.Parser (jsonParser) as JSON
import Data.Either (Either(..))
import Data.Tuple (Tuple(..), snd)
import Data.Tuple.Nested (type (/\), (/\), get1)

import Server.Handler (Request, Response, response, addResponseHeader, updateRequestVal)

type JSONRequest = Request JSON.Json
type JSONResponse = Response JSON.Json

jsonResponse :: forall a. (JSON.EncodeJson a) => HP.Status -> a -> JSONResponse
jsonResponse status = response status <<< JSON.encodeJson

okJsonResponse :: forall a. (JSON.EncodeJson a) => a -> JSONResponse
okJsonResponse = jsonResponse 200

toJsonRequest :: forall a. Request a -> Either String (Request (Tuple JSON.Json a))
toJsonRequest {headers, httpVersion, method, path, query, body, val} = do
  json <- JSON.jsonParser body
  pure {headers, httpVersion, method, path, query, body, val: Tuple json val}

fromJsonResponse :: Response JSON.Json -> Response String
fromJsonResponse {headers, status, body} =
  addResponseHeader "Content-Type" "application/json" $
  {headers, status, body: JSON.stringify body}

wrapJsonRequest :: forall a res. (String -> res) -> (Request (Tuple JSON.Json a) -> res) -> Request a -> res
wrapJsonRequest parseFail router req = case toJsonRequest req of
  (Left err) -> parseFail err
  (Right jsonRequest) -> router jsonRequest

wrapDecodeJson :: forall a b res. (JSON.DecodeJson a)
                  => (String -> res)
                  -> (Request (a /\ b) -> res)
                  -> Request (JSON.Json /\ b) -> res
wrapDecodeJson errorHandler router req = case JSON.decodeJson $ get1 req.val of
  (Left err) -> errorHandler err
  (Right updatedVal) -> router $ updateRequestVal (const (updatedVal /\ snd req.val)) req

wrapJsonResponse :: forall req m. (Bind m) => (Applicative m) => (req -> m (Response JSON.Json)) -> req -> m (Response String)
wrapJsonResponse router request = do
  res <- router request
  pure $ fromJsonResponse res
