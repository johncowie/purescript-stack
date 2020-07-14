module Utils.ExceptT
( showError
, mapError
, module Control.Monad.Except.Trans
, right
, liftEffectRight
, booleanToError
)
where
import Prelude
import Control.Monad.Except.Trans (ExceptT(..), runExceptT)

import Data.Either (Either(..))

import Effect (Effect)
import Effect.Class (liftEffect, class MonadEffect)

mapError :: forall a b c. (a -> c) -> Either a b -> Either c b
mapError f (Left v) = Left (f v)
mapError f (Right v) = Right v

showError :: forall e a. (Show e) => Either e a -> Either String a
showError (Left err) = Left $ show err
showError (Right v) = Right v

right :: forall e m a. (Functor m) => m a -> ExceptT e m a
right vM = ExceptT $ Right <$> vM

liftEffectRight :: forall e m a. (MonadEffect m) => Effect a -> ExceptT e m a
liftEffectRight vM = ExceptT $ liftEffect $ Right <$> vM

booleanToError :: forall err. err -> Boolean -> Either err Unit
booleanToError err b = if b then Right unit else Left err
