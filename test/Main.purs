module Test.Main where

import Prelude

import Data.Either (Either(..))

import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class.Console as Console
import Effect.Exception.Unsafe (unsafeThrow)

import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

import Server.DB.Test as DB.Test
import Server.DB as DB

import Utils.ExceptT (ExceptT(..), liftEffectRight, runExceptT, showError)


throwError :: Effect (Either String Unit) -> Effect Unit
throwError eff = do
  e <- eff
  case e of
    (Left err) -> unsafeThrow err
    (Right v) -> pure v

main :: Effect Unit
main = throwError $ runExceptT do
  pool <- ExceptT $ (showError <$> DB.getDB "postgres://localhost:5432/events_store")
  liftEffectRight $ launchAff_ $ runSpec [consoleReporter] do
    DB.Test.main pool
