module Env2 where

import Data.Maybe (Maybe(..))
import Data.Either (Either(..))

type EnvVarParser a
  = { description :: String
    , var :: String
    , default :: Maybe a
    , parser :: String -> Either String a
    }

{-
Some kind of setup where

fromEnv :: {val :: EnvVarParser a} -> Either String {val :: a}

config :: {
  a: var "An environment variable" "A_VARIABLE" (Just "2.3")
}

-}
