module Utils.ExceptT
( mapErrorT
, module Control.Monad.Except.Trans
, module Effect.Exception
, right
, liftEffectRight
, booleanToError
)
where
import Prelude
import Control.Monad.Except.Trans (ExceptT(..), runExceptT)

import Data.Bifunctor (lmap)
import Data.Either (Either(..))

import Effect (Effect)
import Effect.Class (liftEffect, class MonadEffect)
import Effect.Exception (Error, error)

mapErrorT :: forall a b c m. (Functor m) => (a -> c) -> ExceptT a m b -> ExceptT c m b
mapErrorT f et = ExceptT $ map (lmap f) $ runExceptT et

right :: forall e m a. (Functor m) => m a -> ExceptT e m a
right vM = ExceptT $ Right <$> vM

liftEffectRight :: forall e m a. (MonadEffect m) => Effect a -> ExceptT e m a
liftEffectRight vM = ExceptT $ liftEffect $ Right <$> vM

booleanToError :: forall err. err -> Boolean -> Either err Unit
booleanToError err b = if b then Right unit else Left err
