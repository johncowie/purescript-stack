module Utils.Alert
( alert )
where

import Prelude
import Effect (Effect)
import Web.HTML (window) as DOM
import Web.HTML.Window (alert) as DOM

alert :: String -> Effect Unit
alert msg = do
  window <- DOM.window
  DOM.alert msg window
