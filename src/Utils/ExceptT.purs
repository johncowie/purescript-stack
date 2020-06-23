module Utils.ExceptT where

import Prelude
import Data.Either (Either(..))

showError :: forall e a. (Show e) => Either e a -> Either String a
showError (Left err) = Left $ show err
showError (Right v) = Right v
