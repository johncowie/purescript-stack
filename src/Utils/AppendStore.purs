module Utils.AppendStore
( AppendStore
, localStorageAppendStore
, httpAppendStore
, syncAppendStore)
where

import Prelude
import Effect (Effect)
import Effect.Aff (Aff)
import Affjax as AX
import Affjax.ResponseFormat as ResponseFormat
import Affjax.RequestBody as RequestBody
import Control.Monad.Except.Trans (ExceptT(..), runExceptT)
import Data.Argonaut.Decode (class DecodeJson, decodeJson) as JSON
import Data.Argonaut.Encode (class EncodeJson, encodeJson) as JSON
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Array (length, reverse)
import Effect.Exception (Error, error)
import Effect.Exception.Unsafe (unsafeThrow)
import Utils.LocalJsonStorage as LS
import Utils.Async (async)

type AppendStore e = {
  append :: e -> Aff (Either Error Unit)
, retrieveAll :: Aff (Either Error (Array e))
}

appendToLocalStorage :: forall e. (JSON.DecodeJson e) => (JSON.EncodeJson e) => String -> e -> Effect (Either Error Unit)
appendToLocalStorage s e = do
  eventsE <- retrieveFromLocalStorage s
  case eventsE of
    (Left err) -> pure (Left err)
    (Right events) -> Right <$> LS.store s ([e] <> events)

retrieveFromLocalStorage :: forall e. (JSON.DecodeJson e) => String -> Effect (Either Error (Array e))
retrieveFromLocalStorage s = do
  eventsEM <- LS.load s
  pure $ fromMaybe [] <$> eventsEM

localStorageAppendStore :: forall e. (JSON.DecodeJson e) => (JSON.EncodeJson e) => String -> AppendStore e
localStorageAppendStore k = {
  append: \e -> async $ appendToLocalStorage k e
, retrieveAll: async $ retrieveFromLocalStorage k
}

appendHTTP :: forall e. (JSON.EncodeJson e) => String -> e -> Aff (Either Error Unit)
appendHTTP s event = do
  result <- AX.post ResponseFormat.json ("http://lvh.me:8080?app=" <> s) body
  case result of
    Left err -> pure $ Left $ error $ AX.printError err
    Right response -> pure $ Right unit
  where body = Just $ RequestBody.json $ JSON.encodeJson event

syncHTTP :: forall e. (JSON.EncodeJson e) => String -> Array e -> Aff (Either Error Unit)
syncHTTP s events = do
  result <- AX.post ResponseFormat.json ("http://lvh.me:8080/sync?app=" <> s) body
  case result of
    Left err -> pure $ Left $ error $ AX.printError err
    Right response -> pure $ Right unit
  where body = Just $ RequestBody.json $ JSON.encodeJson events

retrieveAllHTTP :: forall e. (JSON.DecodeJson e) => String -> Aff (Either Error (Array e))
retrieveAllHTTP s = do
  result <- AX.get ResponseFormat.json ("http://lvh.me:8080?app=" <> s)
  case result of
    Left err -> pure $ Left $ error $ AX.printError err
    Right response -> case JSON.decodeJson response.body of
      Left jsonErr -> pure $ Left $ error jsonErr
      Right events -> pure $ Right $ events

httpAppendStore :: forall e. (JSON.DecodeJson e) => (JSON.EncodeJson e) => String -> AppendStore e
httpAppendStore k = {
  append: appendHTTP k
, retrieveAll: retrieveAllHTTP k
}

syncAppendStore :: forall e. (JSON.DecodeJson e) => (JSON.EncodeJson e) => String -> AppendStore e
syncAppendStore k = {append, retrieveAll}
  where append event = runExceptT do
          (eventsFromHTTP :: Array e) <- ExceptT $ retrieveAllHTTP k
          (eventsFromLocal :: Array e) <- ExceptT $ async $ retrieveFromLocalStorage k
          ExceptT $ Right <$> (async $ LS.store k ([event] <> eventsFromLocal))
          if length eventsFromHTTP == length eventsFromLocal
            then ExceptT $ appendHTTP k event
            else ExceptT $ syncHTTP k $ reverse ([event] <> eventsFromLocal)
        retrieveAll = async $ retrieveFromLocalStorage k
