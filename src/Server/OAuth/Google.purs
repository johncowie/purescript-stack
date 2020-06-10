module Server.OAuth.Google
( GoogleCode
, GoogleUserData
, loadConfig
, oauth )
where

import Prelude

import Data.Newtype (class Newtype)
import Data.Either (Either)

import Effect.Aff (Aff)
import Effect.Exception.Unsafe (unsafeThrow)

import Foreign.Object (Object)

import Type.Data.Row (RProxy(..))

import TypedEnv (type (<:), fromEnv, EnvError)

import Server.OAuth (OAuth)

newtype GoogleCode = GoogleCode String
derive instance newtypeGoogleCode :: Newtype GoogleCode _

newtype GoogleUserData = GoogleUserData {
  email :: String
, name :: String
}
derive instance newtypeGoogleUserData :: Newtype GoogleUserData _

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

redirect :: GoogleConfig -> String
redirect config = unsafeThrow ""

handleCode :: GoogleConfig -> GoogleCode -> Aff (Either String GoogleUserData)
handleCode code = unsafeThrow ""

oauth :: Object String -> Either EnvError (OAuth GoogleCode GoogleUserData)
oauth env = do
  config <- loadConfig env
  pure { redirect: redirect config
       , handleCode: handleCode config}
