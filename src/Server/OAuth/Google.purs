module Server.OAuth.Google
( GoogleCode
, GoogleUserData
, loadConfig
, oauth )
where

import CustomPrelude

import Affjax.RequestBody as RequestBody

import Data.Newtype (class Newtype, unwrap)
import Data.Either (Either)
import Data.Tuple (Tuple(..))
import Data.Maybe (Maybe(..))
import Data.FormURLEncoded (FormURLEncoded(..))

import URI.Extra.QueryPairs as Query
import URI.Query (print)

import Effect.Aff (Aff)

import Foreign.Object (Object)

import Type.Data.Row (RProxy(..))

import TypedEnv (type (<:), fromEnv, EnvError)

import Server.OAuth (OAuth)
import Server.QueryParams (class ParseQueryParam, parseNewtype)

import Utils.HttpClient as Http
import Utils.ExceptT (ExceptT(..), runExceptT, showError)
import Utils.JWT (JWT, extractPayload)

newtype GoogleCode = GoogleCode String
derive instance newtypeGoogleCode :: Newtype GoogleCode _

instance googleCodeParseQueryParam :: ParseQueryParam GoogleCode where
  parseQueryParam = parseNewtype

type GoogleUserData = {
  sub :: String
, email :: String
, name :: String
}

type GoogleConfigProxy = (
  oauthUrl :: String <: "GOOGLE_OAUTH_URL"
, apiUrl :: String <: "GOOGLE_API_URL"
, clientId :: String <: "GOOGLE_CLIENT_ID"
, clientSecret :: String <: "GOOGLE_CLIENT_SECRET"
)

type GoogleConfig = {
  oauthUrl :: String
, apiUrl :: String
, clientId :: String
, clientSecret :: String
}

loadConfig :: Object String -> Either EnvError GoogleConfig
loadConfig env = fromEnv (RProxy :: RProxy GoogleConfigProxy) env

-- mkRedirectUri ::  -> RedirectQuery -> String

queryString :: Array (Tuple String String) -> String
queryString pairs = print $
                    Query.print identity identity $
                    Query.QueryPairs $
                    map (\(Tuple k v) -> Tuple (Query.keyFromString k) (Just (Query.valueFromString v))) $
                    pairs

formData :: Array (Tuple String String) -> RequestBody.RequestBody
formData tuples = RequestBody.formURLEncoded $ FormURLEncoded $ map (map Just) tuples

{-
Redirect user to google, with query parameters set, e.g. clientID, callback url, etc..
-}
redirect :: String -> GoogleConfig -> String
redirect redirectUri config = config.oauthUrl <> query
  where query = queryString $
                [ Tuple "response_type" "code"
                , Tuple "access_type" "online"
                , Tuple "scope" "profile email"
                , Tuple "prompt" "select_account consent"
                , Tuple "client_id" config.clientId
                , Tuple "redirect_uri" redirectUri
                ]

fetchOpenIdData :: String -> GoogleConfig -> GoogleCode -> Aff (Either String {access_token :: JWT, id_token :: JWT})
fetchOpenIdData redirectUri config code = map showError $ Http.postReturnJson url body
  where url = config.apiUrl <> "/oauth2/v4/token"
        body = formData [
          Tuple "code" (unwrap code)
        , Tuple "client_id" config.clientId
        , Tuple "client_secret" config.clientSecret
        , Tuple "redirect_uri" redirectUri
        , Tuple "grant_type" "authorization_code"
        ]
        -- query =

{-
  code will come in query parameters
  receive the code from Google, and use to make request to fetch user data.

  response type of access code request:
  {access_token: <>
   id_token: JWT token - inside payload is sub (i.e. ID), name and email }

-}
handleCode :: String -> GoogleConfig -> GoogleCode -> Aff (Either String GoogleUserData)
handleCode redirectUri config code = runExceptT do
  tokenData <- ExceptT $ fetchOpenIdData redirectUri config code
  ExceptT $ pure $ extractPayload tokenData.id_token

oauth :: String -> Object String -> Either EnvError (OAuth GoogleCode GoogleUserData)
oauth redirectUri env = do
  config <- loadConfig env
  pure { redirect: redirect redirectUri config
       , handleCode: handleCode redirectUri config}
