module LensDemo where

import Prelude
import Utils.Lens as L
import Data.Symbol (SProxy(..))

type Bedroom = {
  bed :: String
}

type House = {
  bedroom :: Bedroom
}

bedroomL :: L.Lens' House Bedroom
bedroomL = L.prop (SProxy :: SProxy "bedroom")

bedL :: L.Lens' Bedroom String
bedL = L.prop (SProxy :: SProxy "bed")

houseBedL :: L.Lens' House String
houseBedL = bedL <<< bedroomL
