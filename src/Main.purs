module Main where

import Prelude
import Goals.App as Goals
import Couplit.App as Couplit
import Dunbar.App as Dunbar
import Effect (Effect)
import Data.Map as M
import Data.Maybe(Maybe(..))
import Utils.Url as Url

import Utils.TinyUrl as TinyUrl

appFromQuery :: M.Map String String -> Effect Unit
appFromQuery queryParams =
  case M.lookup "app" queryParams of
    (Just "couplit") -> Couplit.runApp
    (Just "dunbar") -> Dunbar.runApp
    (Just "tiny") -> TinyUrl.main
    _ -> Goals.runApp

main :: Effect Unit
main = do
  url <- Url.getWindowUrl
  let app = appFromQuery $ Url.getQueryParams url
  app
