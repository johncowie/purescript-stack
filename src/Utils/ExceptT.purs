module Utils.ExceptT
( showError
, mapError
, module Control.Monad.Except.Trans
)
where
import Prelude
import Control.Monad.Except.Trans (ExceptT(..), runExceptT)
import Data.Either (Either(..))

mapError :: forall a b c. (a -> c) -> Either a b -> Either c b
mapError f (Left v) = Left (f v)
mapError f (Right v) = Right v

showError :: forall e a. (Show e) => Either e a -> Either String a
showError (Left err) = Left $ show err
showError (Right v) = Right v
