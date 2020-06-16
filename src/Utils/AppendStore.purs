module Utils.AppendStore
( AppendStore
-- , localStorageAppendStore
, httpAppendStore
, Snapshot
, SnapshotStore
, httpSnapshotStore
)
where

import Prelude

import Affjax as AX
import Affjax.ResponseFormat as ResponseFormat
import Affjax.RequestBody as RequestBody
import Affjax.RequestHeader (RequestHeader(..))
import Data.Argonaut.Decode (class DecodeJson, decodeJson) as JSON
import Data.Argonaut.Encode (class EncodeJson, encodeJson) as JSON
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.HTTP.Method (Method(..))

import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Console as Console
import Effect.Exception (Error, error)

-- import Utils.LocalJsonStorage as LS

type AppendStore id e = {
  append :: e -> Aff (Either Error Unit)
, retrieveAll :: Aff (Either Error (Array {id :: id, event :: e}))
}

-- appendToLocalStorage :: forall e.
--                         (JSON.DecodeJson e)
--                      => (JSON.EncodeJson e)
--                      => String
--                      -> e
--                      -> Effect (Either Error Unit)
-- appendToLocalStorage s e = do
--   eventsE <- retrieveFromLocalStorage s
--   case eventsE of
--     (Left err) -> pure (Left err)
--     (Right events) -> Right <$> LS.store s ([e] <> events)
--
-- retrieveFromLocalStorage :: forall id e.
--                             (JSON.DecodeJson e)
--                          => (JSON.DecodeJson id)
--                          => String
--                          -> Effect (Either Error (Array {id :: id, event :: e}))
-- retrieveFromLocalStorage s = do
--   eventsEM <- LS.load s
--   pure $ fromMaybe [] <$> eventsEM
--
-- localStorageAppendStore :: forall e. (JSON.DecodeJson e) => (JSON.EncodeJson e) => String -> AppendStore e
-- localStorageAppendStore k = {
--   append: \e -> async $ appendToLocalStorage k e
-- , retrieveAll: async $ retrieveFromLocalStorage k
-- }

apiGet :: forall e. (JSON.DecodeJson e) => String -> Aff (Either Error e)
apiGet url = do
  result <- AX.request $ AX.defaultRequest { responseFormat = ResponseFormat.json
                                           , method = Left GET
                                           , url = url
                                           , headers = [RequestHeader "Authorization" "john:bobbydazzler"]}
  case result of
    Left err -> pure $ Left $ error $ AX.printError err
    Right response -> case JSON.decodeJson response.body of
      Left jsonErr -> pure $ Left $ error jsonErr
      Right events -> pure $ Right $ events

apiGetMaybe :: forall e. (JSON.DecodeJson e) => String -> Aff (Either Error (Maybe e))
apiGetMaybe url = do
  result <- AX.request $ AX.defaultRequest { responseFormat = ResponseFormat.json
                                           , method = Left GET
                                           , url = url
                                           , headers = [RequestHeader "Authorization" "john:bobbydazzler"]}
  case result of
    Left err -> pure $ Left $ error $ AX.printError err
    Right response -> case JSON.decodeJson response.body of
      Left jsonErr -> do
        liftEffect $ Console.log $ "Error parsing json: " <> jsonErr
        pure $ Right Nothing
      Right val -> pure $ Right $ Just val

apiPost :: forall e. (JSON.EncodeJson e) => String -> e -> Aff (Either Error Unit)
apiPost url e = do
  result <- AX.request $ AX.defaultRequest { responseFormat = ResponseFormat.json
                                           , method = Left POST
                                           , url = url
                                           , content = body
                                           , headers = [RequestHeader "Authorization" "john:bobbydazzler"]}
  case result of
    Left err -> pure $ Left $ error $ AX.printError err
    Right response -> pure $ Right unit
  where body = Just $ RequestBody.json $ JSON.encodeJson e

rootUrl :: String
rootUrl = "http://lvh.me:8080"-- "https://dumb-waiter.herokuapp.com"

appendHTTP :: forall e. (JSON.EncodeJson e) => String -> e -> Aff (Either Error Unit)
appendHTTP s event = apiPost (rootUrl <> "?app=" <> s) event

retrieveAllHTTP :: forall id e.
                   (JSON.DecodeJson id)
                => (JSON.DecodeJson e)
                => String
                -> Aff (Either Error (Array {id :: id, event :: e}))
retrieveAllHTTP s = apiGet (rootUrl <> "?app=" <> s)

httpAppendStore :: forall id e.
                   (JSON.DecodeJson e)
                => (JSON.EncodeJson e)
                => (JSON.DecodeJson id)
                => String
                -> AppendStore id e
httpAppendStore k = {
  append: appendHTTP k
, retrieveAll: retrieveAllHTTP k
}

-- snapshot stuff

type Snapshot st = {
  state :: st,
  upToEvent :: Int
}

type SnapshotStore st = {
  retrieveLatestSnapshot :: Aff (Either Error (Maybe (Snapshot st)))
, saveSnapshot :: (Snapshot st) -> Aff (Either Error Unit)
}

retrieveLatestSnapshotHTTP :: forall st. (JSON.DecodeJson st) => String -> Aff (Either Error (Maybe (Snapshot st)))
retrieveLatestSnapshotHTTP app = apiGetMaybe (rootUrl <> "/snapshots?app=" <> app)

saveSnapshotHTTP :: forall st. (JSON.EncodeJson st) => String -> (Snapshot st) -> Aff (Either Error Unit)
saveSnapshotHTTP app = apiPost (rootUrl <> "/snapshots?app=" <> app)

httpSnapshotStore :: forall st. (JSON.DecodeJson st) => (JSON.EncodeJson st) => String -> SnapshotStore st
httpSnapshotStore k = {
  retrieveLatestSnapshot: retrieveLatestSnapshotHTTP k
, saveSnapshot: saveSnapshotHTTP k
}

-- how are snapshots going to work??
-- 1) Load snapshot from database (use empty state if doesn't exist or parse)
-- 2) Load all events after upToEvent and replay on top of state (state needs to keep track of event number)

-- async periodically store
