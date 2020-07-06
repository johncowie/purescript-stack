module Main where

import CustomPrelude

import Api as Api
import Browser.Cookie as Cookie
import Browser.Cookies.Data (SetCookie(..), Cookie(..))
import Couplit.App as Couplit
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap, unwrap)
import Dunbar.App as Dunbar
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Exception.Unsafe (unsafeThrow)
import Experiment as Exp
import Goals.App as Goals
import Spork.Html as H
import Spork.Html.Properties as P
import Spork.PureApp as SPA
import Utils.AppendStore (ApiRoot, ApiConfig)
import Utils.Components.Lib as ComponentLib
import Utils.Url as Url

-- TODO figure out what to do if token call errors
getToken :: ApiRoot -> String -> String -> Aff String
getToken apiRoot googleCode redirect = do
  respE <- Api.getAccessToken apiRoot googleCode redirect
  case respE of
    (Right payload) -> pure (payload.accessToken)
    (Left err) -> unsafeThrow (show err)

-- TODO figure what to do if redirect call fails
getGoogleRedirect :: ApiRoot -> String -> Aff String
getGoogleRedirect apiRoot redirect = do
  respE <- Api.getOAuthRedirect apiRoot redirect
  case respE of
    (Right payload) -> pure (payload.redirect)
    (Left err) -> unsafeThrow (show err)

loginButton :: String -> H.Html Unit
loginButton googleOAuthUrl = H.div [] [H.a [P.href googleOAuthUrl] [H.text "Login"]]

loginPage :: String -> Effect Unit
loginPage googleOAuthUrl = do
  a <- SPA.makeWithSelector app "#app"
  a.run
  where app = {init: unit,
               update,
               render}
        update m a = m
        render m = loginButton googleOAuthUrl

startLogin :: Config -> Aff Unit
startLogin config = do
  redirect <- getGoogleRedirect config.apiRoot (config.frontendRoot)
  liftEffect (loginPage redirect)

setTokenCookie :: String -> String -> Effect Unit
setTokenCookie key value = Cookie.setCookie (SetCookie {cookie, opts})
  where cookie = Cookie {key, value}
        opts = Nothing -- TODO specify maxage etc..?

finishLogin :: Config -> String -> Effect Unit
finishLogin config code = launchAff_ $ do
  token <- getToken config.apiRoot code (config.frontendRoot)
  liftEffect $ setTokenCookie "token" token
  liftEffect $ Url.redirect config.frontendRoot

routeApp :: ApiConfig -> Array String -> Effect Unit
routeApp api path = case path of
  ["couplit"] -> Couplit.runApp
  ["dunbar"] -> Dunbar.runApp (Dunbar.mkConfig api)
  ["exp"] ->  Exp.main
  ["lib"] -> ComponentLib.main
  _ -> Goals.runApp (Goals.mkConfig api)

type FrontEndRoot = String

main_ :: Config -> Effect Unit
main_ config = do
  url <- Url.getWindowUrl
  let path = Url.getPath url
  let codeM = Url.getQueryParam "code" url
  case codeM of
    (Just code) -> finishLogin config url
    Nothing -> do
      cookieM <- Cookie.getCookie "token"
      case cookieM of
        (Just cookie) -> routeApp {url: config.apiRoot, token: wrap (unwrap cookie).value} path
        Nothing -> launchAff_ $ startLogin config

{-
Login App
- Pure App that just needs a login button with the redirect url

Code Handler
- if path has google in it, then use code to get token from oauth, and store token in cookie
-}
  -- pass token and login url to event apps, through config
  --  future TODO if API returns a 401 at any point, then user should be booted back to login
  --  future TODO redirect to where you were trying to get to before

type Config = {
  apiRoot :: ApiRoot
, frontendRoot :: String
}

dev :: Effect Unit
dev = main_ { apiRoot: wrap "http://lvh.me:8080"
            , frontendRoot: "http://lvh.me:1234" }

main :: Effect Unit
main = main_ { apiRoot: wrap "https://dumb-waiter.herokuapp.com"
             , frontendRoot: "https://johncowie.github.io/purescript-stack"}
