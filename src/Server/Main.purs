module Server.Main where

import Prelude

import Control.Monad.Except.Trans (ExceptT(..), runExceptT)

import Data.Symbol (SProxy(..))
import Data.Map as M
import Data.Newtype (wrap, unwrap)
import Data.Argonaut.Core (Json, stringify) as JSON
import Data.Argonaut.Parser (jsonParser) as JSON
import Data.Argonaut.Decode (class DecodeJson, decodeJson) as JSON
import Data.Argonaut.Encode (class EncodeJson, encodeJson) as JSON
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..), snd)
import Data.Tuple.Nested (type (/\), get1, (/\))

import Prim.RowList as RL

import Type.Data.Row (RProxy(..))

import Node.Process (getEnv)

import Effect (Effect)
import Effect.Aff (Aff, runAff)
import Effect.Class (liftEffect)
import Effect.Console as Console

import Utils.Lens as L

import HTTPure as HP
import HTTPure.Headers as Headers
import HTTPure.Version (Version)

import TypedEnv (type (<:), fromEnv)

import Server.DB as DB
import Server.Migrations.MigrationData (migrationStore)
import Server.Migrations.Postgres (executor, intVersionStore)
import Server.Migrations (migrate, Migrator)
import Server.QueryParams (class GDecodeQueryParams, parseQueryParams)
import Server.Domain (AppName, EventId)

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

addToRequestVal :: forall a b. b -> Request a -> Request (b /\ a)
addToRequestVal val = updateRequestVal (Tuple val)

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

retrieveEventsHandler :: DB.Pool
                      -> Request ({app :: AppName, after :: Maybe EventId} /\ Unit)
                      -> Aff (Either String (Response JSON.Json))
retrieveEventsHandler pool req = runExceptT do
  events <- ExceptT $ showError <$> DB.retrieveEvents app after pool
  let eventRecords = map (\(id /\ event) -> {id, event}) events
  pure $ okJsonResponse eventRecords
  where ({app, after} /\ _) = req.val

addEventsHandler :: forall a.
                    DB.Pool
                 -> Request (JSON.Json /\ String /\ a)
                 -> Aff (Either String (Response JSON.Json))
addEventsHandler pool req = do
  result <- showError <$> DB.addEvent eventApp json pool
  pure $ const successResponse <$> result
  where (json /\ eventApp /\ _) = req.val

retrieveSnapshotHandler :: DB.Pool
                        -> Request (String /\ Unit)
                        -> Aff (Either String (Response JSON.Json))
retrieveSnapshotHandler pool req = runExceptT do
  snapshotM <- ExceptT $ showError <$> DB.retrieveLatestSnapshot eventApp pool
  pure $ case snapshotM of
    Just (Tuple state upToEvent) -> okJsonResponse {state, upToEvent}
    Nothing -> okJsonResponse {}
  where (eventApp /\ _) = req.val

addSnapshotHandler :: forall a. DB.Pool
                   -> Request (JSON.Json /\ String /\ a)
                   -> Aff (Either String (Response JSON.Json))
addSnapshotHandler pool req = runExceptT do
  {snapshot, upToEvent} :: {snapshot :: JSON.Json, upToEvent :: Int} <- ExceptT $ pure $ JSON.decodeJson json
  void $ ExceptT $ showError <$> DB.insertSnapshot eventApp snapshot upToEvent pool
  pure successResponse
  where (json /\ eventApp /\ _) = req.val


wrapQueryParam :: forall a res. String -> (String -> Aff res) -> (Request (Tuple String a) -> Aff res) -> Request a -> Aff res
wrapQueryParam key errorHandler handler req =
  case req.query HP.!! key of
    (Just param) -> handler $ updateRequestVal (Tuple param) req
    Nothing -> errorHandler ("Missing query parameter [" <> key <> "]")

wrapOptQueryParam :: forall a res. String -> (Request (Tuple (Maybe String) a) -> Aff res) -> Request a -> Aff res
wrapOptQueryParam key handler req =
  handler $ addToRequestVal (req.query HP.!! key) req

wrapParseQueryParams :: forall a row list res.
                        GDecodeQueryParams row list
                     => RL.RowToList row list
                     => (String -> Aff res)
                     -> (Request (Record row /\ a)
                     -> Aff res)
                     -> Request a
                     -> Aff res
wrapParseQueryParams errorHandler handler req = case parseQueryParams req.query of
  (Left err) -> errorHandler err
  (Right qp) -> handler $ addToRequestVal qp req

wrapCors :: forall req a. (req -> Aff (Response a)) -> req -> Aff (Response a)
wrapCors router req = do
  res <- router req
  pure $ addResponseHeader "Access-Control-Allow-Origin" "*" $
         addResponseHeader "Access-Control-Allow-Headers" "*" $ res

wrapBasicAuth :: forall res a.
                 String
              -> String
              -> (String -> Aff res)
              -> (Request a -> Aff res)
              -> Request a
              -> Aff res
wrapBasicAuth username password errorHandler router req =
  if isAuthed
    then router req
    else errorHandler "Unauthorized"
  where isAuthed = fromMaybe false $ do
          authHeader <- M.lookup (wrap "Authorization") (unwrap req.headers)
          pure $ authHeader == username <> ":" <> password

successResponse :: JSONResponse
successResponse = okJsonResponse $ JSON.encodeJson {message: "Success"}

showError :: forall a b. (Show a) => Either a b -> Either String b
showError (Left e) = Left (show e)
showError (Right e) = Right e

lookupHandler :: DB.Pool -> Request Unit -> (Request Unit -> Aff (Response String))
lookupHandler pool req@{method: HP.Options} = const $ pure $ response 200 ""
lookupHandler pool req@{method: HP.Get, path: ["snapshots"]} =
  wrapBasicAuth "john" "bobbydazzler" plainErrorHandler $
  wrapJsonResponse $
  wrapQueryParam "app" jsonBadRequestHandler $
  wrapResponseErrors jsonErrorHandler $
  retrieveSnapshotHandler pool
lookupHandler pool req@{method: HP.Post, path: ["snapshots"]} =
  wrapBasicAuth "john" "bobbydazzler" plainErrorHandler $
  wrapJsonResponse $
  wrapQueryParam "app" jsonBadRequestHandler $
  wrapJsonRequest jsonBadRequestHandler $
  wrapResponseErrors jsonErrorHandler $
  addSnapshotHandler pool
lookupHandler pool req@{method: HP.Get, path: []} =
  wrapBasicAuth "john" "bobbydazzler" plainErrorHandler $
  wrapJsonResponse $
  wrapParseQueryParams jsonBadRequestHandler $
  wrapResponseErrors jsonErrorHandler $
  retrieveEventsHandler pool
lookupHandler pool req@{method: HP.Post, path: []} =
  wrapBasicAuth "john" "bobbydazzler" plainErrorHandler $
  wrapJsonResponse $
  wrapQueryParam "app" jsonBadRequestHandler $
  wrapJsonRequest jsonBadRequestHandler $
  wrapResponseErrors jsonErrorHandler $
  addEventsHandler pool
lookupHandler pool req =
  wrapJsonResponse $
  const (pure $ jsonResponse 404 {response: "not found"})

baseRouter :: DB.Pool -> Request Unit -> Aff (Response String)
baseRouter pool req = (lookupHandler pool req) req

wrapCustom :: (Request Unit -> Aff (Response String)) -> HP.Request -> Aff HP.Response
wrapCustom router request = do
  res <- router (toCustomRequest request)
  fromCustomResponse res

plainErrorHandler :: String -> Aff (Response String)
plainErrorHandler msg = pure $ response 401 msg

app :: DB.Pool -> HP.Request -> HP.ResponseM
app pool =
  wrapCustom $
  wrapCors $
  baseRouter pool

type Config = (
  port :: Maybe Int <: "PORT"
, databaseUri :: Maybe String <: "DATABASE_URI"
)

logError :: Aff (Either String Unit) -> Aff Unit
logError e = do
  r <- e
  case r of
    (Left err) -> liftEffect $ Console.log $ "ERROR: " <> err
    _ -> pure unit

migrator :: DB.Pool -> Migrator Aff Int String
migrator pool =
  { executor: executor pool
  , migrationStore
  , versionStore: intVersionStore pool
  , logger: liftEffect <<< Console.log
  }

affErrorHandler :: forall e a. (Show e) => Either e a -> Effect Unit
affErrorHandler (Left err) = do
  Console.log $ "ERROR: " <> show err
affErrorHandler _ = pure unit

-- | Boot up the server
main :: Effect Unit -- HP.ServerM
main = void $ runAff affErrorHandler $ logError $ runExceptT $ do
  config <- ExceptT $ showError <$> (liftEffect $ fromEnv (RProxy :: RProxy Config) <$> getEnv)
  let port = fromMaybe 8080 config.port
      hostname = "0.0.0.0"
      backlog = Nothing
      dbUri = fromMaybe "postgres://localhost:5432/events_store" config.databaseUri
  pool <- ExceptT $ showError <$> (liftEffect $ DB.getDB dbUri)
  ExceptT $ migrate $ migrator pool
  void $ ExceptT $ liftEffect $ Right <$> (HP.serve' {port, backlog, hostname} (app pool) do
    Console.log $ " ┌────────────────────────────────────────────┐"
    Console.log $ " │ Server now up on port " <> show port <> "                 │"
    Console.log $ " └────────────────────────────────────────────┘")
