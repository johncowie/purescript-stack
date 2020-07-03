module Main where

import Prelude

import Browser.Cookie as Cookie
import Browser.Cookies.Data (SetCookie(..), Cookie(..))
import Couplit.App as Couplit
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap, unwrap)
import Dunbar.App as Dunbar
import Effect (Effect)
import Experiment as Exp
import Goals.App as Goals
import Utils.AppendStore (ApiRoot, ApiConfig)
import Utils.Components.Lib as ComponentLib
import Utils.Url as Url

routeApp :: String -> ApiConfig -> Effect Unit
routeApp url api = case Array.head $ Url.getPath url of
  (Just "couplit") -> Couplit.runApp
  (Just "dunbar") -> Dunbar.runApp (Dunbar.mkConfig api)
  (Just "exp") ->  Exp.main
  (Just "lib") -> ComponentLib.main
  _ -> Goals.runApp (Goals.mkConfig api)

setTokenCookie :: String -> String -> Effect Unit
setTokenCookie key value = Cookie.setCookie (SetCookie {cookie, opts})
  where cookie = Cookie {key, value}
        opts = Nothing -- TODO specify maxage etc..?

main_ :: ApiRoot -> Effect Unit
main_ api = do
  url <- Url.getWindowUrl
  case Url.getQueryParam "token" url of
    (Just token) -> setTokenCookie "token" token
    Nothing -> pure unit

  cookieM <- Cookie.getCookie "token"
  case cookieM of
    (Just cookie) -> routeApp url {url: api, token: wrap (unwrap cookie).value}
    Nothing -> routeApp url {url: api, token: wrap "blah"} -- TODO trigger login

  -- pass token and login url to event apps, through config
  --  future TODO if API returns a 401 at any point, then user should be booted back to login

dev :: Effect Unit
dev = main_ (wrap "http://lvh.me:8080")

main :: Effect Unit
main = main_ (wrap "https://dumb-waiter.herokuapp.com")
