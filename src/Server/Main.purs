module Server.Main where

import CustomPrelude

import Affjax.RequestBody as RequestBody

import Data.Map as M
import Data.Newtype (wrap, unwrap)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (decodeJson)
import Data.Maybe (fromMaybe)
import Data.Tuple (Tuple(..))
import Data.Array (drop)
import Data.Tuple.Nested (type (/\), (/\))

import Type.Data.Row (RProxy(..))

import Node.Process as NP

import Effect (Effect)
import Effect.Aff (Aff, runAff)
import Effect.Class (liftEffect)
import Effect.Console as Console
import Effect.Exception (Error)

import HTTPure as HP

import TypedEnv (type (<:), fromEnv)

import Server.DB as DB
import Server.Migrations.MigrationData (migrationStore)
import Server.Migrations.Postgres (executor, intVersionStore)
import Server.Migrations (migrate, Migrator)
import Server.Domain (AppName, EventId)
import Server.Handler (Request, Response, addResponseHeader, response, wrapCustom, redirect)
import Server.Middleware.JSON (JSONResponse)
import Server.Middleware.JSON as JSON
import Server.Middleware.QueryParams (wrapParseQueryParams)
import Server.OAuth (OAuth)
import Server.OAuth.Google (GoogleCode, GoogleUserData)
import Server.OAuth.Google as Google

import Utils.ExceptT (ExceptT(..), runExceptT, showError, liftEffectRight)

import Utils.HttpClient as Http

retrieveEventsHandler :: DB.Pool
                      -> Request ({app :: AppName, after :: Maybe EventId} /\ Unit)
                      -> Aff (Either String JSONResponse)
retrieveEventsHandler pool req = runExceptT do
  events <- ExceptT $ showError <$> DB.retrieveEvents app after pool
  let eventRecords = map (\(id /\ event) -> {id, event}) events
  pure $ JSON.okJsonResponse eventRecords
  where ({app, after} /\ _) = req.val

addEventsHandler :: forall a.
                    DB.Pool
                 -> Request (Json /\ {app :: AppName} /\ a)
                 -> Aff (Either String JSONResponse) -- TODO easier to read if you can see what the result type is
addEventsHandler pool req = runExceptT do
  eventId <- ExceptT $ showError <$> DB.addEvent query.app json pool
  pure $ JSON.okJsonResponse {id: eventId}
  where (json /\ query /\ _) = req.val

retrieveSnapshotHandler :: DB.Pool
                        -> Request ({app :: AppName} /\ Unit)
                        -> Aff (Either String JSONResponse)
retrieveSnapshotHandler pool req = runExceptT do
  snapshotM <- ExceptT $ showError <$> DB.retrieveLatestSnapshot query.app pool
  pure $ case snapshotM of
    Just (Tuple state upToEvent) -> JSON.okJsonResponse {state, upToEvent}
    Nothing -> JSON.okJsonResponse {}
  where (query /\ _) = req.val

addSnapshotHandler :: forall a. DB.Pool
                   -> Request (Json /\ {app :: AppName} /\ a)
                   -> Aff (Either String JSONResponse)
addSnapshotHandler pool req = runExceptT do
  {state, upToEvent} :: {state :: Json, upToEvent :: Int} <- ExceptT $ pure $ decodeJson json
  void $ ExceptT $ showError <$> DB.insertSnapshot query.app state upToEvent pool
  pure successResponse
  where (json /\ query /\ _) = req.val

oauthLoginHandler :: forall req m oacode oadata.
                     (Applicative m)
                  => (OAuth oacode oadata)
                  -> req
                  -> m (Response String)
oauthLoginHandler oauth _ = pure $ redirect oauth.redirect

-- FIXME this is timing out
googleCodeHandler :: OAuth GoogleCode GoogleUserData
                  -> Request ({code :: GoogleCode} /\ Unit)
                  -> Aff (Either String JSONResponse)
googleCodeHandler oauth req = runExceptT $ do
  userData <- ExceptT $ oauth.handleCode code
  pure $ JSON.okJsonResponse userData
  where ({code} /\ _) = req.val

stubUserData :: forall req. req -> Aff JSONResponse
stubUserData _ = pure $ JSON.okJsonResponse { sub: "123", name: "Bob", email: "bob@bob.com"}

affjaxHandler :: forall req. req -> Aff (Response String)
affjaxHandler _ = do
  (respE :: Either Error {sub :: String, name :: String, email :: String}) <- Http.postReturnJson "http://lvh.me:8080/stub" (RequestBody.string "blah")
  case respE of
    (Left err) -> pure $ response 500 $ show err
    (Right resp) -> pure $ response 200 $ "RESULT: "  <> show resp

-- Some rando bits of middleware
wrapResponseErrors :: forall req res. (String -> Aff res) -> (req -> Aff (Either String res)) -> req -> Aff res
wrapResponseErrors errorHandler router request = do
  responseE :: Either String res <- router request
  case responseE of
    (Left err) -> do
      liftEffect $ Console.error err
      errorHandler err
    (Right res) -> pure res

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

wrapLogRequest :: forall a res.
                 (Request a -> Aff res)
               -> Request a
               -> Aff res
wrapLogRequest router req = do
  liftEffect $ Console.log $ show req.method <> ": " <> show req.path
  router req

---

successResponse :: JSONResponse
successResponse = JSON.okJsonResponse {message: "Success"}

type Dependencies = {
  db :: DB.Pool
, oauth :: {google :: OAuth GoogleCode GoogleUserData}
}

lookupHandler :: Dependencies -> HP.Method -> Array String -> (Request Unit -> Aff (Response String))
lookupHandler deps HP.Options _ = const $ pure $ response 200 ""
lookupHandler deps HP.Get ["login"] = oauthLoginHandler deps.oauth.google
lookupHandler deps _ ["stub"] = JSON.wrapJsonResponse stubUserData
lookupHandler deps HP.Get ["affjax"] = affjaxHandler
lookupHandler deps HP.Get ["google"] =
  JSON.wrapJsonResponse $
  wrapParseQueryParams JSON.jsonBadRequestHandler $
  wrapResponseErrors JSON.jsonErrorHandler $
  googleCodeHandler deps.oauth.google
lookupHandler deps HP.Get ["snapshots"] =
  wrapBasicAuth "john" "bobbydazzler" plainErrorHandler $
  JSON.wrapJsonResponse $
  wrapParseQueryParams JSON.jsonBadRequestHandler $
  wrapResponseErrors JSON.jsonErrorHandler $
  retrieveSnapshotHandler deps.db
lookupHandler deps HP.Post ["snapshots"] =
  wrapBasicAuth "john" "bobbydazzler" plainErrorHandler $
  JSON.wrapJsonResponse $
  wrapParseQueryParams JSON.jsonBadRequestHandler $
  JSON.wrapJsonRequest JSON.jsonBadRequestHandler $
  wrapResponseErrors JSON.jsonErrorHandler $
  addSnapshotHandler deps.db
lookupHandler deps HP.Get [] =
  wrapBasicAuth "john" "bobbydazzler" plainErrorHandler $
  JSON.wrapJsonResponse $
  wrapParseQueryParams JSON.jsonBadRequestHandler $
  wrapResponseErrors JSON.jsonErrorHandler $
  retrieveEventsHandler deps.db
lookupHandler deps HP.Post [] =
  wrapBasicAuth "john" "bobbydazzler" plainErrorHandler $
  JSON.wrapJsonResponse $
  wrapParseQueryParams JSON.jsonBadRequestHandler $
  JSON.wrapJsonRequest JSON.jsonBadRequestHandler $
  wrapResponseErrors JSON.jsonErrorHandler $
  addEventsHandler deps.db
lookupHandler deps _ _ =
  JSON.wrapJsonResponse $
  const (pure $ JSON.jsonResponse 404 {response: "not found"})

baseRouter :: Dependencies -> Request Unit -> Aff (Response String)
baseRouter deps req = (lookupHandler deps req.method req.path) req

plainErrorHandler :: String -> Aff (Response String)
plainErrorHandler msg = pure $ response 401 msg

app :: Dependencies -> HP.Request -> HP.ResponseM
app deps =
  wrapCustom $
  wrapLogRequest $
  wrapCors $
  baseRouter deps

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

data Mode = Dev | Prod

instance showMode :: Show Mode where
  show Dev = "Development"
  show Prod = "Production"

modeFromArgs :: Array String -> Mode
modeFromArgs ["dev"] = Dev
modeFromArgs _ = Prod

-- TODO this is a bit of a hack, should really replace oauth component with stubbed version instead
injectStubVars :: Mode -> Effect Unit
injectStubVars Prod = pure unit
injectStubVars Dev = do
  NP.setEnv "GOOGLE_OAUTH_URL" "blah"
  NP.setEnv "GOOGLE_API_URL" "blah"
  NP.setEnv "GOOGLE_CLIENT_ID" "blah"
  NP.setEnv "GOOGLE_CLIENT_SECRET" "blah"


-- | Boot up the server
main :: Effect Unit
main = void $ runAff affErrorHandler $ logError $ runExceptT $ do
  args <- liftEffectRight NP.argv
  let mode = modeFromArgs $ drop 2 args
  liftEffectRight $ injectStubVars mode
  env <- liftEffectRight NP.getEnv
  config <- ExceptT $ pure $ showError $ fromEnv (RProxy :: RProxy Config) env
  let port = fromMaybe 8080 config.port
      hostname = "0.0.0.0"
      backlog = Nothing
      dbUri = fromMaybe "postgres://localhost:5432/events_store" config.databaseUri
  pool <- ExceptT $ showError <$> (liftEffect $ DB.getDB dbUri)
  ExceptT $ migrate $ migrator pool
  googleOAuth <- ExceptT $ pure $ showError $ Google.oauth "https://dumb-waiter.herokuapp.com/google" env
  let deps = {db: pool, oauth: {google: googleOAuth}}
  void $ ExceptT $ liftEffect $ Right <$> (HP.serve' {port, backlog, hostname} (app deps) do
    Console.log $ " ┌────────────────────────────────────────────┐"
    Console.log $ " │ Server now up on port " <> show port <> "                 │"
    Console.log $ " └────────────────────────────────────────────┘"
    Console.log $ "Mode: " <> show mode
    )
