module Utils.AppendStore
( AppendStore
-- , localStorageAppendStore
, httpAppendStore
, Snapshot
, SnapshotStore
, httpSnapshotStore
, ApiRoot
, ApiToken
, ApiConfig
)
where

import Prelude

import Control.Monad.Except.Trans (ExceptT(..), runExceptT)

import Affjax as AX
import Affjax.ResponseFormat as ResponseFormat
import Affjax.RequestBody as RequestBody
import Affjax.RequestHeader (RequestHeader(..))

import Data.Argonaut.Decode (class DecodeJson, decodeJson) as JSON
import Data.Argonaut.Encode (class EncodeJson, encodeJson) as JSON
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.HTTP.Method (Method(..))
import Data.Newtype (class Newtype, unwrap)

import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Console as Console
import Effect.Exception (Error, error)

-- import Utils.LocalJsonStorage as LS

type AppendStore id e = {
  append :: e -> Aff (Either Error id)
, retrieveAll :: Aff (Either Error (Array {id :: id, event :: e}))
, retrieveAfter :: Int -> Aff (Either Error (Array {id :: id, event :: e}))
}

data Ignored = Ignored

newtype ApiRoot = ApiRoot String
derive instance newtypeApiRoot :: Newtype ApiRoot _

newtype ApiToken = ApiToken String
derive instance newtypeApiToken :: Newtype ApiToken _

type ApiConfig = {url :: ApiRoot, token :: ApiToken}

instance decodeJsonIgnored :: JSON.DecodeJson Ignored where
  decodeJson s = Right Ignored

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

apiGet :: forall e. (JSON.DecodeJson e) => ApiConfig -> String -> Aff (Either Error e)
apiGet config path = do
  result <- AX.request $ AX.defaultRequest { responseFormat = ResponseFormat.json
                                           , method = Left GET
                                           , url = unwrap config.url <> path
                                           , headers = [RequestHeader "AuthToken" (unwrap config.token)]}
  case result of
    Left err -> pure $ Left $ error $ AX.printError err
    Right response -> case JSON.decodeJson response.body of
      Left jsonErr -> pure $ Left $ error jsonErr
      Right events -> pure $ Right $ events

apiGetMaybe :: forall e. (JSON.DecodeJson e) => ApiConfig -> String -> Aff (Either Error (Maybe e))
apiGetMaybe config path = do
  result <- AX.request $ AX.defaultRequest { responseFormat = ResponseFormat.json
                                           , method = Left GET
                                           , url = unwrap config.url <> path
                                           , headers = [RequestHeader "AuthToken" (unwrap config.token)]}
  case result of
    Left err -> pure $ Left $ error $ AX.printError err
    Right response -> case JSON.decodeJson response.body of
      Left jsonErr -> do
        liftEffect $ Console.log $ "Error parsing json: " <> jsonErr
        pure $ Right Nothing
      Right val -> pure $ Right $ Just val

apiPost :: forall v e. (JSON.EncodeJson e) => (JSON.DecodeJson v) => ApiConfig -> String -> e -> Aff (Either Error v)
apiPost config path e = do
  result <- AX.request $ AX.defaultRequest { responseFormat = ResponseFormat.json
                                           , method = Left POST
                                           , url = unwrap config.url <> path
                                           , content = body
                                           , headers = [RequestHeader "AuthToken" (unwrap config.token)]}
  case result of
    Left err -> pure $ Left $ error $ AX.printError err
    Right response -> case JSON.decodeJson response.body of
      Left jsonErr -> do
        pure $ Left $ error jsonErr
      Right val -> pure $ Right val
  where body = Just $ RequestBody.json $ JSON.encodeJson e

appendHTTP :: forall id e. (JSON.EncodeJson e)
           => (JSON.DecodeJson id)
           => ApiConfig
           -> String
           -> e
           -> Aff (Either Error id)
appendHTTP config appId event = do
  (resE :: Either Error {id :: id}) <- apiPost config ("?app=" <> appId) event
  pure $ _.id <$> resE

retrieveAllHTTP :: forall id e.
                   (JSON.DecodeJson id)
                => (JSON.DecodeJson e)
                => ApiConfig
                -> String
                -> Aff (Either Error (Array {id :: id, event :: e}))
retrieveAllHTTP config appId = apiGet config ("?app=" <> appId)

retrieveAfterHTTP :: forall id e.
                     (JSON.DecodeJson id)
                  => (JSON.DecodeJson e)
                  => ApiConfig
                  -> String
                  -> Int
                  -> Aff (Either Error (Array {id :: id, event :: e}))
retrieveAfterHTTP config appId eventId = apiGet config ("?app=" <> appId <> "&after=" <> show eventId)

httpAppendStore :: forall id e.
                   (JSON.DecodeJson e)
                => (JSON.EncodeJson e)
                => (JSON.DecodeJson id)
                => ApiConfig
                -> String
                -> AppendStore id e
httpAppendStore config appId = {
  append: appendHTTP config appId
, retrieveAll: retrieveAllHTTP config appId
, retrieveAfter: retrieveAfterHTTP config appId
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

retrieveLatestSnapshotHTTP :: forall st. (JSON.DecodeJson st)
                           => ApiConfig
                           -> String
                           -> Aff (Either Error (Maybe (Snapshot st)))
retrieveLatestSnapshotHTTP config app = apiGetMaybe config ("/snapshots?app=" <> app)

saveSnapshotHTTP :: forall st. (JSON.EncodeJson st)
                 => ApiConfig
                 -> String
                 -> (Snapshot st)
                 -> Aff (Either Error Unit)
saveSnapshotHTTP config app snapshot = runExceptT do
  (result :: Ignored) <- ExceptT $ apiPost config ("/snapshots?app=" <> app) snapshot
  pure unit

httpSnapshotStore :: forall st. (JSON.DecodeJson st)
                  => (JSON.EncodeJson st)
                  => ApiConfig
                  -> String
                  -> SnapshotStore st
httpSnapshotStore config app = {
  retrieveLatestSnapshot: retrieveLatestSnapshotHTTP config app
, saveSnapshot: saveSnapshotHTTP config app
}

-- how are snapshots going to work??
-- 1) Load snapshot from database (use empty state if doesn't exist or parse)
-- 2) Load all events after upToEvent and replay on top of state (state needs to keep track of event number)

-- async periodically store
