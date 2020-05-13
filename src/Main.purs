module Main where

import Prelude
import Goals.App as Goals
import Couplit.App as Couplit
import Dunbar.App as Dunbar
import Utils.Components.Lib as ComponentLib
import Effect (Effect)
import Data.Maybe(Maybe(..))
import Data.Array as Array
import Utils.Url as Url

import Utils.TinyUrl as TinyUrl
import Experiment as Exp

routeApp :: String -> Effect Unit
routeApp url = case Array.head $ Url.getPath url of
  (Just "couplit") -> Couplit.runApp
  (Just "dunbar") -> Dunbar.runApp
  (Just "tiny") -> TinyUrl.main
  (Just "exp") ->  Exp.main
  (Just "lib") -> ComponentLib.main
  _ -> Goals.runApp

main :: Effect Unit
main = do
  url <- Url.getWindowUrl
  routeApp url
