module Server.Main where

import CustomPrelude

import Affjax.RequestBody as RequestBody

import Data.Map as M
import Data.Newtype (wrap, unwrap)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Encode (encodeJson)
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
import Server.Domain (AppName, EventId, OAuthProvider(Google), UserId)
import Server.Handler (Request, Response, addResponseHeader, response, wrapCustom, redirect)
import Server.Middleware.JSON (JSONResponse)
import Server.Middleware.JSON as JSON
import Server.Middleware.QueryParams (wrapParseQueryParams)
import Server.Middleware.Auth as AuthM

import Server.OAuth (OAuth)
import Server.OAuth.Google (GoogleCode, GoogleUserData)
import Server.OAuth.Google as Google

import Utils.ExceptT (ExceptT(..), runExceptT, showError, liftEffectRight)
import Utils.JWT (JWTGenerator, jwtGenerator)

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

oauthLoginHandler :: forall m oacode oadata.
                     (Applicative m)
                  => (OAuth oacode oadata)
                  -> Request ({redirect :: String} /\ Unit)
                  -> m (Response {redirect :: String})
oauthLoginHandler oauth req = pure $ response 200 {redirect: oauth.redirect query.redirect}
  where (query /\ _) = req.val

googleCodeHandler :: OAuth GoogleCode GoogleUserData
                  -> DB.Pool
                  -> JWTGenerator {sub :: UserId}
                  -> Request ({code :: GoogleCode, redirect :: String} /\ Unit)
                  -> Aff (Either String JSONResponse)
googleCodeHandler oauth db tokenGen req = runExceptT $ do
  userData <- ExceptT $ oauth.handleCode code redirect
  let newUser = {thirdParty: Google, thirdPartyId: userData.sub, name: userData.name}
  userId <- ExceptT $ map showError $ DB.upsertUser newUser db
  token <- liftEffectRight $ tokenGen.generate {sub: userId}
  pure $ JSON.okJsonResponse {accessToken: token}
  where ({code, redirect} /\ _) = req.val

testAuthHandler :: forall a. AuthM.AuthedRequest {sub :: UserId} a
                -> Aff JSONResponse
testAuthHandler authedReq = pure $ JSON.okJsonResponse {msg: "Successfully authed!", userId: userId}
  where userId = _.sub $ AuthM.tokenPayload authedReq

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
, tokenGen :: JWTGenerator {sub :: UserId}
}

lookupHandler :: Dependencies -> HP.Method -> Array String -> (Request Unit -> Aff (Response String))
lookupHandler deps HP.Options _ = const $ pure $ response 200 ""
lookupHandler deps HP.Get ["login"] =
  wrapParseQueryParams (JSON.wrapJsonResponse jsonBadRequestHandler) $
  JSON.wrapJsonResponse $
  oauthLoginHandler deps.oauth.google
lookupHandler deps _ ["stub"] = JSON.wrapJsonResponse stubUserData
lookupHandler deps HP.Get ["affjax"] = affjaxHandler
lookupHandler deps HP.Get ["google"] =
  JSON.wrapJsonResponse $
  wrapParseQueryParams jsonBadRequestHandler $
  wrapResponseErrors jsonErrorHandler $
  googleCodeHandler deps.oauth.google deps.db deps.tokenGen
lookupHandler deps HP.Get ["snapshots"] =
  JSON.wrapJsonResponse $
  wrapParseQueryParams jsonBadRequestHandler $
  wrapResponseErrors jsonErrorHandler $
  retrieveSnapshotHandler deps.db
lookupHandler deps HP.Post ["snapshots"] =
  JSON.wrapJsonResponse $
  wrapParseQueryParams jsonBadRequestHandler $
  JSON.wrapJsonRequest jsonBadRequestHandler $
  wrapResponseErrors jsonErrorHandler $
  addSnapshotHandler deps.db
lookupHandler deps HP.Get ["test-auth"] =
  JSON.wrapJsonResponse $
  AuthM.wrapTokenAuth deps.tokenGen.verifyAndExtract jsonAuthErrorHandler $
  testAuthHandler
lookupHandler deps HP.Get [] =
  JSON.wrapJsonResponse $
  wrapParseQueryParams jsonBadRequestHandler $
  wrapResponseErrors jsonErrorHandler $
  retrieveEventsHandler deps.db
lookupHandler deps HP.Post [] =
  JSON.wrapJsonResponse $
  wrapParseQueryParams jsonBadRequestHandler $
  JSON.wrapJsonRequest jsonBadRequestHandler $
  wrapResponseErrors jsonErrorHandler $
  addEventsHandler deps.db
lookupHandler deps _ _ =
  JSON.wrapJsonResponse $
  const (pure $ JSON.jsonResponse 404 {response: "not found"})

baseRouter :: Dependencies -> Request Unit -> Aff (Response String)
baseRouter deps req = (lookupHandler deps req.method req.path) req

plainErrorHandler :: String -> Aff (Response String)
plainErrorHandler msg = pure $ response 401 msg

jsonErrorHandler :: String -> Aff (Response Json)
jsonErrorHandler error = liftEffect $ do
  Console.error error
  pure $ JSON.jsonResponse 500 $ encodeJson {error: "Server error"}

jsonBadRequestHandler :: String -> Aff (Response Json)
jsonBadRequestHandler error = liftEffect $ do
  Console.log $ "Bad request: " <> error
  pure $ JSON.jsonResponse 400 $ encodeJson {error}

jsonAuthErrorHandler :: String -> Aff (Response Json)
jsonAuthErrorHandler error = liftEffect $ do
  Console.log $ "Auth error: " <> error
  pure $ JSON.jsonResponse 401 $ encodeJson {error: "Authorization required"}

app :: Dependencies -> HP.Request -> HP.ResponseM
app deps =
  wrapCustom $
  wrapLogRequest $
  wrapCors $
  baseRouter deps

type Config = (
  port :: Maybe Int <: "PORT"
, databaseUri :: Maybe String <: "DATABASE_URI"
, jwtSecret :: String <: "JWT_SECRET"
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
  NP.setEnv "JWT_SECRET" "devsecret"

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
  googleOAuth <- ExceptT $ pure $ showError $ Google.oauth env
  let deps = {
    db: pool,
    oauth: {google: googleOAuth},
    tokenGen: jwtGenerator config.jwtSecret
  }
  ExceptT $ migrate $ migrator pool
  void $ ExceptT $ liftEffect $ Right <$> (HP.serve' {port, backlog, hostname} (app deps) do
    Console.log $ " ┌────────────────────────────────────────────┐"
    Console.log $ " │ Server now up on port " <> show port <> "                 │"
    Console.log $ " └────────────────────────────────────────────┘"
    Console.log $ "Mode: " <> show mode
    )
