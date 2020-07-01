module Server.DB.Test where

import CustomPrelude

import Control.Monad.Error.Class (class MonadThrow)

import Data.Newtype (wrap)

import Test.Spec (Spec, pending, describe, it, beforeAll)
import Test.Spec.Assertions (shouldEqual, shouldNotEqual, fail)

import Effect(Effect)
import Effect.Class
import Effect.Console as Console
import Effect.Aff (Aff, launchAff_)
import Effect.Exception (Error, error)

import Database.PostgreSQL.PG as PG

import Server.DB as DB
import Server.Domain (OAuthProvider(Stub))
import Utils.ExceptT (ExceptT(..), runExceptT, showError, mapError)

shouldBeRight :: forall a b m. (Show a) => (MonadThrow Error m) => Either a b -> m Unit
shouldBeRight (Left v) = fail $ "Expected Right but was Left: " <> show v
shouldBeRight (Right _) = pure unit

failOnError :: forall e a m. (Show e) => (MonadThrow Error m) => m (Either e a) -> m Unit
failOnError eff = do
  vE <- eff
  case vE of
    (Left err) -> fail $ show err
    (Right _) -> pure unit

main :: PG.Pool -> Spec Unit
main db = describe "db" do
    describe "upserting user" do
      it "can retrieve empty events" do
        failOnError $ DB.retrieveEvents (wrap "anApp") Nothing db
      it "should save new user" do
        failOnError $ runExceptT do
          let user1 = {thirdParty: Stub, thirdPartyId: "123", name: "Bob"}
              user1Updated = {thirdParty: Stub, thirdPartyId: "123", name: "Bill"}
              user2 = {thirdParty: Stub, thirdPartyId: "234", name: "Geoff"}
          userId1 <- ExceptT $ map (mapError (show >>> error)) $ DB.upsertUser user1 db
          userId2 <- ExceptT $ map (mapError (show >>> error)) $ DB.upsertUser user1Updated db
          userId3 <- ExceptT $ map (mapError (show >>> error)) $ DB.upsertUser user2 db
          userId1 `shouldEqual` userId2
          userId2 `shouldNotEqual`userId3
