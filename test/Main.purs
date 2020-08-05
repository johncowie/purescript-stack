module Test.Main where

import Prelude
import Data.Either (Either(..))
import Data.Bifunctor (lmap)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Exception.Unsafe (unsafeThrow)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)
import Server.DBTest as DBTest
import Dunbar.StateTest as DunbarStateTest
import Server.DB as DB
import Utils.ExceptT (ExceptT(..), liftEffectRight, runExceptT)

throwError :: Effect (Either String Unit) -> Effect Unit
throwError eff = do
  e <- eff
  case e of
    (Left err) -> unsafeThrow err
    (Right v) -> pure v

main :: Effect Unit
main =
  throwError
    $ runExceptT do
        pool <- ExceptT $ (lmap show <$> DB.getDB "postgres://localhost:5432/events_store")
        liftEffectRight $ launchAff_
          $ runSpec [ consoleReporter ] do
              DBTest.main pool
              DunbarStateTest.main
