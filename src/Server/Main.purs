module Server.Main where

import Prelude

import Data.Array as Array
import Data.Symbol (SProxy(..))
import Data.Map as M
import Data.Newtype (wrap)
import Data.Argonaut.Core (Json, stringify) as JSON
import Data.Argonaut.Parser (jsonParser) as JSON
import Data.Argonaut.Decode (class DecodeJson, decodeJson) as JSON
import Data.Argonaut.Encode (class EncodeJson, encodeJson) as JSON
import Data.DateTime.Instant (fromDateTime)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..), fst, snd)
import Data.Tuple.Nested (type (/\), (/\), get1, get2)

import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Console as Console

import Utils.Lens as L
import Utils.DateTime as UDT
import Utils.IdMap (Id(..))

import HTTPure as HP
import HTTPure.Headers as Headers
import HTTPure.Version (Version)

import Server.DB as DB
import Goals.Data.Event as Goals


_headers :: forall r. L.Lens' {headers :: HP.Headers | r} HP.Headers
_headers = L.prop (SProxy :: SProxy "headers")

type Request a = { headers :: HP.Headers
                 , httpVersion :: Version
                 , method :: HP.Method
                 , path :: HP.Path
                 , query :: HP.Query
                 , body :: String
                 , val :: a }

-- todo derive functor

type Response a = { headers :: HP.Headers
                  , status :: HP.Status
                  , body :: a }

type JSONRequest = Request JSON.Json
type JSONResponse = Response JSON.Json

updateRequestVal :: forall a b. (a -> b) -> Request a -> Request b
updateRequestVal f {headers, httpVersion, method, path, query, body, val}
  = {headers, httpVersion, method, path, query, body, val: (f val)}

updateRequestValM :: forall a m b. (Bind m) => (Applicative m) => (a -> m b) -> Request a -> m (Request b)
updateRequestValM f {headers, httpVersion, method, path, query, body, val} = do
  updated <- f val
  pure {headers, httpVersion, method, path, query, body, val: updated}

response :: forall a. HP.Status -> a -> Response a
response status body = {headers: Headers.empty, status, body}

emptyResponse :: HP.Status -> Response Unit
emptyResponse status = response status unit

jsonResponse :: forall a. (JSON.EncodeJson a) => HP.Status -> a -> JSONResponse
jsonResponse status = response status <<< JSON.encodeJson

okJsonResponse :: forall a. (JSON.EncodeJson a) => a -> JSONResponse
okJsonResponse = jsonResponse 200

toCustomRequest :: HP.Request -> Request Unit
toCustomRequest {headers, httpVersion, method, path, query, body}
  = {headers, httpVersion, method, path, query, body, val: unit}

fromCustomResponse :: Response String -> Aff HP.Response
fromCustomResponse r = do
  res <- HP.response r.status r.body
  pure $ res {headers = r.headers}

toJsonRequest :: forall a. Request a -> Either String (Request (Tuple JSON.Json a))
toJsonRequest {headers, httpVersion, method, path, query, body, val} = do
  json <- JSON.jsonParser body
  pure {headers, httpVersion, method, path, query, body, val: Tuple json val}

addResponseHeader :: forall r. String -> String -> {headers :: HP.Headers | r} -> {headers :: HP.Headers | r}
addResponseHeader k v = L.over (_headers >>> L._newtype) (M.insert (wrap k) v)

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

wrapJsonResponse :: forall req. (req -> Aff (Response JSON.Json)) -> req -> Aff (Response String)
wrapJsonResponse router request = do
  res <- router request
  pure $ fromJsonResponse res

wrapResponseErrors :: forall req res. (String -> Aff res) -> (req -> Aff (Either String res)) -> req -> Aff res
wrapResponseErrors errorHandler router request = do
  responseE :: Either String res <- router request
  case responseE of
    (Left err) -> do
      liftEffect $ Console.error err
      errorHandler err
    (Right res) -> pure res

jsonErrorHandler :: String -> Aff (Response JSON.Json)
jsonErrorHandler error = pure $ jsonResponse 500 json
  where json = JSON.encodeJson {error}

jsonBadRequestHandler :: String -> Aff (Response JSON.Json)
jsonBadRequestHandler error = pure $ jsonResponse 400 json
  where json = JSON.encodeJson {error}

sampleEvents :: Array Goals.Event
sampleEvents = Array.reverse $
  [ Goals.addGoalEvent "Wanking" (UDT.dateToDateTime $ UDT.newDate 2020 1 1) (UDT.dateToDateTime $ UDT.newDate 2021 1 1) 100
  , Goals.addProgressEvent (Id 0) (fromDateTime $ UDT.dateToDateTime $ UDT.newDate 2020 2 2) 20.0 "Great wank" ]

retrieveEventsHandler :: Request (Tuple String Unit) -> Aff (Either String (Response JSON.Json))
retrieveEventsHandler req = do
  events <- showError <$> DB.retrieveEvents eventApp
  pure $ okJsonResponse <$> events
  where eventApp = fst req.val

wrapGetQueryParam :: forall a res. String -> (String -> Aff res) -> (Request (Tuple String a) -> Aff res) -> Request a -> Aff res
wrapGetQueryParam key errorHandler router req =
  case req.query HP.!! key of
    (Just param) -> router $ updateRequestVal (Tuple param) req
    Nothing -> errorHandler ("Missing query parameter [" <> key <> "]")

wrapCors :: forall req a. (req -> Aff (Response a)) -> req -> Aff (Response a)
wrapCors router req = do
  res <- router req
  pure $ addResponseHeader "Access-Control-Allow-Origin" "*" $
         addResponseHeader "Access-Control-Allow-Headers" "*" $ res

successResponse :: JSONResponse
successResponse = okJsonResponse $ JSON.encodeJson {message: "Success"}

showError :: forall a b. (Show a) => Either a b -> Either String b
showError (Left e) = Left (show e)
showError (Right e) = Right e

addEventsHandler :: forall a. Request (JSON.Json /\ String /\ a) -> Aff (Either String (Response JSON.Json))
addEventsHandler req = do
  result <- showError <$> DB.addEvent eventApp json
  pure $ const successResponse <$> result
  where eventApp = get2 req.val
        json = get1 req.val

syncEventsHandler :: forall a. Request (Array JSON.Json /\ String /\ a) -> Aff (Either String (Response JSON.Json))
syncEventsHandler req = do
  result <- showError <$> DB.syncAll eventApp jsonArr
  pure $ const successResponse <$> result
  where eventApp = get2 req.val
        jsonArr = get1 req.val

baseRouter :: Request Unit -> Aff (Response String)
baseRouter req@{method: HP.Get} = (wrapJsonResponse $
                                   wrapGetQueryParam "app" jsonBadRequestHandler $
                                   wrapResponseErrors jsonErrorHandler $
                                   retrieveEventsHandler)
                                   req
baseRouter req@{method: HP.Options} = pure $ response 200 ""
baseRouter req@{method: HP.Post, path: ["sync"]} =
  (wrapJsonResponse $
   wrapGetQueryParam "app" jsonBadRequestHandler $
   wrapJsonRequest jsonBadRequestHandler $
   wrapDecodeJson jsonBadRequestHandler $ 
   wrapResponseErrors jsonErrorHandler $
   syncEventsHandler)
   req
baseRouter req@{method: HP.Post} = (wrapJsonResponse $
                                    wrapGetQueryParam "app" jsonBadRequestHandler $
                                    wrapJsonRequest jsonBadRequestHandler $
                                    wrapResponseErrors jsonErrorHandler $
                                    addEventsHandler)
                                    req
baseRouter req = (wrapJsonResponse $
                 const (pure $ jsonResponse 404 {response: "not found"})) req

wrapCustom :: (Request Unit -> Aff (Response String)) -> HP.Request -> Aff HP.Response
wrapCustom router request = do
  res <- router (toCustomRequest request)
  fromCustomResponse res

app :: HP.Request -> HP.ResponseM
app = wrapCustom $
      wrapCors $
      baseRouter

-- | Boot up the server
main :: HP.ServerM
main = HP.serve 8080 app do
  Console.log $ " ┌────────────────────────────────────────────┐"
  Console.log $ " │ Server now up on port 8080                 │"
  Console.log $ " │                                            │"
  Console.log $ " │ To test, run:                              │"
  Console.log $ " │  > curl localhost:8080   # => hello world! │"
  Console.log $ " └────────────────────────────────────────────┘"
