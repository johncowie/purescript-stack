module Server.OAuth.Google
( loadConfig
, oauth )
where

import CustomPrelude

import Affjax.RequestBody as RequestBody

import Data.Newtype (unwrap)
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

import Server.OAuth (OAuth, OAuthCode, UserData)

import Utils.HttpClient as Http
import Utils.ExceptT (ExceptT(..), runExceptT, showError)
import Utils.JWT (JWT, extractPayload)

-- TODO could surely write something that looks better than this?
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
redirect :: GoogleConfig -> String -> String
redirect config redirectUri = config.oauthUrl <> query
  where query = queryString $
                [ Tuple "response_type" "code"
                , Tuple "access_type" "online"
                , Tuple "scope" "profile email"
                , Tuple "prompt" "select_account consent"
                , Tuple "client_id" config.clientId
                , Tuple "redirect_uri" redirectUri
                ]

fetchOpenIdData :: GoogleConfig -> String -> OAuthCode -> Aff (Either String {access_token :: JWT, id_token :: JWT})
fetchOpenIdData config redirectUri code = map showError $ Http.postReturnJson url body
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
handleCode :: GoogleConfig -> OAuthCode -> String -> Aff (Either String UserData)
handleCode config code redirectUri = runExceptT do
  tokenData <- ExceptT $ fetchOpenIdData config redirectUri code
  ExceptT $ pure $ extractPayload tokenData.id_token

oauth :: Object String -> Either EnvError OAuth
oauth env = do
  config <- loadConfig env
  pure { redirect: redirect config
       , handleCode: handleCode config}
