module Server.Main where

import Prelude
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (decodeJson)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Bifunctor (lmap)
import Data.Array (drop)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Effect.Aff (Aff, runAff)
import Effect.Class (liftEffect)
import Effect.Console as Console
import HTTPure as HP
import Node.Process as NP
import Server.DB as DB
import Server.Domain (AppName, EventId, OAuthProvider(Google), UserId)
import JohnCowie.HTTPure.Middleware.FormURLEncoded as Form
import JohnCowie.HTTPure.Middleware.JSON (JSONResponse)
import JohnCowie.HTTPure.Middleware.JSON as JSON
import JohnCowie.HTTPure.Middleware.QueryParams (wrapParseQueryParams)
import Server.Middleware.TwilioAuth as TwilioAuth
import JohnCowie.Migrations (migrate, Migrator)
import JohnCowie.PostgreSQL.Migrations (executor, intVersionStore)
import JohnCowie.PostgreSQL (DB, getDB)
import Server.Migrations.MigrationData (migrationStore)
import JohnCowie.OAuth (OAuth, OAuthCode)
import JohnCowie.OAuth.Google as Google
import JohnCowie.OAuth.Stub as StubOAuth
import Server.ChatBot.WhatsApp (WhatsAppBot)
import Dunbar.ChatBot (dunbarWhatsAppBot)
import JohnCowie.HTTPure.Middleware.Auth as AuthM
import JohnCowie.HTTPure (class IsRequest, BasicRequest, Response, addResponseHeader, response, serve', setContentType)
import JohnCowie.HTTPure as Req
import Twilio.Config (TwilioConfig, twilioEnvVars)
import Twilio.Twiml as Twiml
import Twilio.WhatsApp (WhatsAppMessage, WhatsAppNumber, WhatsAppDeliveryNotification)
import Twilio.WhatsApp as WA
import Utils.ExceptT (ExceptT(..), runExceptT, liftEffectRight)
import JohnCowie.JWT (JWTGenerator, jwtGenerator)
import Utils.Lens as L
import Envisage (describe, defaultTo, readEnv, showParsed)
import Envisage.Var (var)
import Envisage.Console (printErrorsForConsole)

type AuthedRequest a
  = AuthM.AuthedRequest { sub :: UserId } a

retrieveEventsHandler ::
  DB ->
  AuthedRequest ({ app :: AppName, after :: Maybe EventId } /\ Unit) ->
  Aff (Either String JSONResponse)
retrieveEventsHandler pool req =
  runExceptT do
    events <- ExceptT $ (lmap show) <$> DB.retrieveEvents sub app after pool
    let
      eventRecords = map (\(id /\ (event :: Json)) -> { id, event }) events
    pure $ JSON.okJsonResponse eventRecords
  where
  ({ app, after } /\ _) = L.view Req._val req

  { sub } = AuthM.tokenPayload req

addEventsHandler ::
  forall a.
  DB ->
  AuthedRequest (Json /\ { app :: AppName } /\ a) ->
  Aff (Either String JSONResponse) -- TODO easier to read if you can see what the result type is
addEventsHandler pool req =
  runExceptT do
    eventId <- ExceptT $ (lmap show) <$> DB.addEvent sub query.app json pool
    pure $ JSON.okJsonResponse { id: eventId }
  where
  (json /\ query /\ _) = L.view Req._val req

  { sub } = AuthM.tokenPayload req

retrieveSnapshotHandler ::
  DB ->
  AuthedRequest ({ app :: AppName } /\ Unit) ->
  Aff (Either String JSONResponse)
retrieveSnapshotHandler pool req =
  runExceptT do
    snapshotM <- ExceptT $ (lmap show) <$> DB.retrieveLatestSnapshot sub query.app pool
    pure
      $ case snapshotM of
          Just (Tuple (state :: Json) upToEvent) -> JSON.okJsonResponse { state, upToEvent }
          Nothing -> JSON.okJsonResponse {}
  where
  (query /\ _) = L.view Req._val req

  { sub } = AuthM.tokenPayload req

addSnapshotHandler ::
  forall a.
  DB ->
  AuthedRequest (Json /\ { app :: AppName } /\ a) ->
  Aff (Either String JSONResponse)
addSnapshotHandler pool req =
  runExceptT do
    { state, upToEvent } :: { state :: Json, upToEvent :: EventId } <- ExceptT $ pure $ decodeJson json
    void $ ExceptT $ (lmap show) <$> DB.insertSnapshot sub query.app state upToEvent pool
    pure successResponse
  where
  (json /\ query /\ _) = L.view Req._val req

  { sub } = AuthM.tokenPayload req

oauthLoginHandler ::
  forall m.
  (Applicative m) =>
  OAuth ->
  BasicRequest ({ redirect :: String } /\ Unit) ->
  m (Response { redirect :: String })
oauthLoginHandler oauth req = pure $ response 200 { redirect: oauth.redirect query.redirect }
  where
  (query /\ _) = L.view Req._val req

googleCodeHandler ::
  OAuth ->
  DB ->
  JWTGenerator { sub :: UserId } ->
  BasicRequest ({ code :: OAuthCode, redirect :: String } /\ Unit) ->
  Aff (Either String JSONResponse)
googleCodeHandler oauth db tokenGen req =
  runExceptT
    $ do
        userData <- ExceptT $ oauth.handleCode code redirect
        let
          newUser = { thirdParty: Google, thirdPartyId: userData.sub, name: userData.name }
        userId <- ExceptT $ map (lmap show) $ DB.upsertUser newUser db
        token <- liftEffectRight $ tokenGen.generate { sub: userId }
        pure $ JSON.okJsonResponse { accessToken: token }
  where
  ({ code, redirect } /\ _) = L.view Req._val req

testAuthHandler ::
  forall a.
  AuthedRequest a ->
  Aff JSONResponse
testAuthHandler authedReq = pure $ JSON.okJsonResponse { msg: "Successfully authed!", userId: userId }
  where
  userId = _.sub $ AuthM.tokenPayload authedReq

messageResponder :: WhatsAppNumber -> String -> String
messageResponder _ msg = "You said '" <> msg <> "' you crazy bastard."

whatsAppMessageHandler ::
  WhatsAppBot Aff ->
  TwilioAuth.TwilioRequest (WhatsAppMessage /\ Unit) ->
  Aff (Either String (Response String))
whatsAppMessageHandler bot req =
  runExceptT do
    reply <- ExceptT $ bot.handleMessage whatsAppMessage
    let
      twiml = Twiml.toString $ WA.toTwimlMaybe $ reply
    liftEffectRight $ Console.log $ "TWIML: " <> twiml
    pure $ setContentType "text/xml" $ response 200 twiml
  where
  (whatsAppMessage /\ _) = L.view Req._val req

whatsAppMessageStatusHandler ::
  TwilioAuth.TwilioRequest (WhatsAppDeliveryNotification /\ Unit) ->
  Aff (Response String)
whatsAppMessageStatusHandler req = do
  liftEffect $ Console.log $ "Status: " <> show dn
  pure $ response 200 "Thanks"
  where
  (dn /\ _) = L.view Req._val req

testQueryParamsHandler ::
  BasicRequest ({ a :: Int, b :: String, c :: String } /\ Unit) ->
  Aff (Response String)
testQueryParamsHandler req = pure $ response 200 "Good job"

stubUserData :: forall req. req -> Aff JSONResponse
stubUserData _ = pure $ JSON.okJsonResponse { sub: "123", name: "Bob", email: "bob@bob.com" }

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
  pure $ addResponseHeader "Access-Control-Allow-Origin" "*"
    $ addResponseHeader "Access-Control-Allow-Headers" "*"
    $ res

wrapLogRequest ::
  forall a req res.
  (IsRequest req) =>
  (req a -> Aff res) ->
  req a ->
  Aff res
wrapLogRequest router req = do
  liftEffect $ Console.log $ show (L.view Req._method req) <> ": " <> show (L.view Req._path req)
  router req

wrapLogRequestBody ::
  forall req a res.
  (IsRequest req) =>
  (req a -> Aff res) ->
  req a ->
  Aff res
wrapLogRequestBody router req = do
  liftEffect $ Console.log $ "Request body: " <> body
  router req
  where
  body = L.view Req._body req

---
successResponse :: JSONResponse
successResponse = JSON.okJsonResponse { message: "Success" }

type Dependencies
  = { db :: DB
    , oauth :: { google :: OAuth }
    , tokenGen :: JWTGenerator { sub :: UserId }
    , twilioConfig :: TwilioConfig -- close over into object (a la oauth)
    , dunbarBot :: WhatsAppBot Aff
    }

lookupHandler :: Dependencies -> HP.Method -> Array String -> (BasicRequest Unit -> Aff (Response String))
lookupHandler deps HP.Options _ = const $ pure $ response 200 ""

lookupHandler deps HP.Get [ "login" ] =
  wrapParseQueryParams (JSON.wrapJsonResponse jsonBadRequestHandler)
    $ JSON.wrapJsonResponse
    $ oauthLoginHandler deps.oauth.google

lookupHandler deps _ [ "stub" ] = JSON.wrapJsonResponse stubUserData

lookupHandler deps HP.Get [ "google" ] =
  JSON.wrapJsonResponse
    $ wrapParseQueryParams jsonBadRequestHandler
    $ wrapResponseErrors jsonErrorHandler
    $ googleCodeHandler deps.oauth.google deps.db deps.tokenGen

lookupHandler deps HP.Get [ "snapshots" ] =
  JSON.wrapJsonResponse
    $ AuthM.wrapTokenAuth deps.tokenGen.verifyAndExtract jsonAuthErrorHandler
    $ wrapParseQueryParams jsonBadRequestHandler
    $ wrapResponseErrors jsonErrorHandler
    $ retrieveSnapshotHandler deps.db

lookupHandler deps HP.Post [ "snapshots" ] =
  JSON.wrapJsonResponse
    $ AuthM.wrapTokenAuth deps.tokenGen.verifyAndExtract jsonAuthErrorHandler
    $ wrapParseQueryParams jsonBadRequestHandler
    $ JSON.wrapJsonRequest jsonBadRequestHandler
    $ wrapResponseErrors jsonErrorHandler
    $ addSnapshotHandler deps.db

lookupHandler deps HP.Get [] =
  JSON.wrapJsonResponse
    $ AuthM.wrapTokenAuth deps.tokenGen.verifyAndExtract jsonAuthErrorHandler
    $ wrapParseQueryParams jsonBadRequestHandler
    $ wrapResponseErrors jsonErrorHandler
    $ retrieveEventsHandler deps.db

lookupHandler deps HP.Post [] =
  JSON.wrapJsonResponse
    $ AuthM.wrapTokenAuth deps.tokenGen.verifyAndExtract jsonAuthErrorHandler
    $ wrapParseQueryParams jsonBadRequestHandler
    $ JSON.wrapJsonRequest jsonBadRequestHandler
    $ wrapResponseErrors jsonErrorHandler
    $ addEventsHandler deps.db

lookupHandler deps HP.Get [ "test-auth" ] =
  JSON.wrapJsonResponse
    $ AuthM.wrapTokenAuth deps.tokenGen.verifyAndExtract jsonAuthErrorHandler
    $ testAuthHandler

lookupHandler deps HP.Post [ "whatsapp" ] =
  Form.wrapFormURLEncoded badRequestHandler
    $ TwilioAuth.wrapTwilioAuth deps.twilioConfig authErrorHandler
    $ Form.wrapDecodeFormURLEncoded badRequestHandler
    $ wrapResponseErrors serverErrorHandler
    $ whatsAppMessageHandler deps.dunbarBot

lookupHandler deps HP.Post [ "whatsapp", "status" ] =
  wrapLogRequestBody
    $ Form.wrapFormURLEncoded badRequestHandler
    $ TwilioAuth.wrapTwilioAuth deps.twilioConfig authErrorHandler
    $ Form.wrapDecodeFormURLEncoded badRequestHandler
    $ whatsAppMessageStatusHandler

lookupHandler deps HP.Get [ "query" ] =
  wrapParseQueryParams (JSON.wrapJsonResponse jsonBadRequestHandler)
    $ testQueryParamsHandler

lookupHandler deps _ _ =
  JSON.wrapJsonResponse
    $ const (pure $ JSON.jsonResponse 404 { response: "not found" })

baseRouter :: Dependencies -> BasicRequest Unit -> Aff (Response String)
baseRouter deps req = (lookupHandler deps method path) req
  where
  method = L.view Req._method req

  path = L.view Req._path req

plainErrorHandler :: String -> Aff (Response String)
plainErrorHandler msg = pure $ response 401 msg

jsonErrorHandler :: String -> Aff (Response Json)
jsonErrorHandler error =
  liftEffect
    $ do
        Console.error error
        pure $ JSON.jsonResponse 500 $ encodeJson { error: "Server error" }

serverErrorHandler :: String -> Aff (Response String)
serverErrorHandler = JSON.wrapJsonResponse jsonErrorHandler

jsonBadRequestHandler :: forall err. (Show err) => (EncodeJson err) => err -> Aff (Response Json)
jsonBadRequestHandler errors =
  liftEffect
    $ do
        Console.log $ "Bad request: " <> show errors
        pure $ JSON.jsonResponse 400 $ encodeJson { errors }

badRequestHandler :: String -> Aff (Response String)
badRequestHandler = JSON.wrapJsonResponse jsonBadRequestHandler

jsonAuthErrorHandler :: String -> Aff (Response Json)
jsonAuthErrorHandler error =
  liftEffect
    $ do
        Console.log $ "Auth error: " <> error
        pure $ JSON.jsonResponse 401 $ encodeJson { error: "Authorization required" }

authErrorHandler :: String -> Aff (Response String)
authErrorHandler = JSON.wrapJsonResponse jsonAuthErrorHandler

app :: Dependencies -> BasicRequest Unit -> Aff (Response String)
app deps =
  wrapLogRequest
    $ wrapCors
    $ baseRouter deps

logError :: Aff (Either String Unit) -> Aff Unit
logError e = do
  r <- e
  case r of
    (Left err) -> liftEffect $ Console.error $ "Startup error:\n" <> err
    _ -> pure unit

migrator :: DB -> Migrator Aff Int String
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

data Mode
  = Dev
  | Prod

instance showMode :: Show Mode where
  show Dev = "Development"
  show Prod = "Production"

modeFromArgs :: Array String -> Mode
modeFromArgs [ "dev" ] = Dev

modeFromArgs _ = Prod

-- TODO this is a bit of a hack, should really replace oauth component with stubbed version instead
--  or have a dev mode file that isn't checked in
injectStubVars :: Mode -> Effect Unit
injectStubVars Prod = pure unit

injectStubVars Dev = do
  NP.setEnv "JWT_SECRET" "devsecret"
  NP.setEnv "TWILIO_ACCOUNT_ID" "blah"
  NP.setEnv "TWILIO_AUTH_TOKEN" "blah"

oauthForMode :: Mode -> OAuth -> OAuth
oauthForMode Prod oauth = oauth
oauthForMode Dev _ = StubOAuth.oauth

-- | Boot up the server
main :: Effect Unit
main =
  void $ runAff affErrorHandler $ logError $ runExceptT
    $ do
        args <- liftEffectRight NP.argv
        let
          mode = modeFromArgs $ drop 2 args
        liftEffectRight $ injectStubVars mode
        env <- liftEffectRight NP.getEnv
        -- type configEnvVars
        --   = ( port :: Maybe Int <: "PORT"
        --     , databaseUri :: Maybe String <: "DATABASE_URI"
        --     , jwtSecret :: String <: "JWT_SECRET"
        --     )
        config <- ExceptT $ pure $ (lmap printErrorsForConsole) $ readEnv
                    { google: Google.googleEnvVars
                    , twilio: twilioEnvVars
                    , port: var "PORT" # defaultTo 8080 # showParsed # describe "Port that app handles HTTP requests on"
                    , databaseUri: var "DATABASE_URI" # showParsed # describe "Postgres connnection URI" # defaultTo "postgres://localhost:5432/events_store"
                    , jwtSecret: var "JWT_SECRET" # showParsed # describe "Secret used for issuing JWT tokens"
                    }
                    env
        let
          port = config.port
          hostname = "0.0.0.0"
          backlog = Nothing
        pool <- ExceptT $ (lmap show) <$> (liftEffect $ getDB config.databaseUri)
        let
          dunbarBot = dunbarWhatsAppBot pool
        let
          deps =
            { db: pool
            , oauth: { google: oauthForMode mode (Google.oauth config.google) }
            , tokenGen: jwtGenerator config.jwtSecret
            , twilioConfig: config.twilio
            , dunbarBot
            }
        ExceptT $ migrate $ migrator pool
        void $ ExceptT $ liftEffect $ Right
          <$> ( serve' { port, backlog, hostname } (app deps) do
                Console.log $ " ┌────────────────────────────────────────────┐"
                Console.log $ " │ Server now up on port " <> show port <> "                 │"
                Console.log $ " └────────────────────────────────────────────┘"
                Console.log $ "Mode: " <> show mode
            )
