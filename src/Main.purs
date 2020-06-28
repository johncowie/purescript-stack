module Main where

import Prelude
import Goals.App as Goals
import Couplit.App as Couplit
import Dunbar.App as Dunbar
import Utils.Components.Lib as ComponentLib
import Demo.AffApp as AffApp
import Effect (Effect)
import Data.Maybe(Maybe(..))
import Data.Array as Array
import Data.Newtype (wrap)
import Utils.Url as Url
import Utils.AppendStore (ApiRoot)

import Experiment as Exp

routeApp :: String -> ApiRoot -> Effect Unit
routeApp url api = case Array.head $ Url.getPath url of
  (Just "couplit") -> Couplit.runApp
  (Just "dunbar") -> Dunbar.runApp (Dunbar.mkConfig api)
  (Just "exp") ->  Exp.main
  (Just "lib") -> ComponentLib.main
  (Just "aff") -> AffApp.main
  _ -> Goals.runApp (Goals.mkConfig api)

main_ :: ApiRoot -> Effect Unit
main_ api = do
  url <- Url.getWindowUrl
  routeApp url api

dev :: Effect Unit
dev = main_ (wrap "http://lvh.me:8080")

main :: Effect Unit
main = main_ (wrap "https://dumb-waiter.herokuapp.com")
