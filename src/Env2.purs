module Env2 where

import Data.Maybe(Maybe(..))
import Data.Either (Either(..))

type EnvParser a = { description :: String
                   , var :: String
                   , default :: Maybe a
                   , parser :: String -> Either String a}
