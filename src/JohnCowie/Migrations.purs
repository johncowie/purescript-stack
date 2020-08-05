module JohnCowie.Migrations where

import Prelude
import Control.Monad.Except.Trans (ExceptT(..), runExceptT)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Array (sortWith, dropWhile, takeWhile, last)
import Data.Traversable (for)

type Migration id a
  = { id :: id
    , description :: String
    , up :: a
    , down :: a
    }

type VersionStore m id a
  = { currentVersion :: m (Either String (Maybe id))
    , updateVersion :: Boolean -> Migration id a -> m (Either String Unit)
    }

type MigrationStore m id a
  = { loadMigrations :: m (Either String (Array (Migration id a)))
    }

type Executor m id a
  = { executeMigration :: id -> a -> m (Either String Unit)
    }

type Migrator m id a
  = { versionStore :: VersionStore m id a
    , executor :: Executor m id a
    , migrationStore :: MigrationStore m id a
    , logger :: String -> m Unit
    }

remainingMigrations :: forall id a. (Ord id) => (Maybe id) -> Array (Migration id a) -> Array (Migration id a)
remainingMigrations Nothing migrations = migrations

remainingMigrations (Just id) migrations = dropWhile (\r -> r.id <= id) $ sortWith _.id migrations

previousMigration :: forall id a. (Ord id) => (Maybe id) -> Array (Migration id a) -> Maybe (Migration id a)
previousMigration Nothing _ = Nothing

previousMigration (Just id) migrations = last $ takeWhile (\r -> r.id <= id) $ sortWith _.id migrations

runMigration :: forall m id a. (Monad m) => Migrator m id a -> Migration id a -> m (Either String Unit)
runMigration { versionStore, executor } migration =
  runExceptT do
    ExceptT $ executor.executeMigration migration.id migration.up
    ExceptT $ versionStore.updateVersion true migration

runRollback :: forall m id a. (Monad m) => Migrator m id a -> Migration id a -> m (Either String Unit)
runRollback { versionStore, executor } migration =
  runExceptT do
    ExceptT $ executor.executeMigration migration.id migration.down
    ExceptT $ versionStore.updateVersion false migration

migrationIds :: forall id a. Array (Migration id a) -> Array id
migrationIds = map _.id

migrate :: forall m id a. (Show id) => (Ord id) => (Monad m) => Migrator m id a -> m (Either String Unit)
migrate migrator@{ versionStore, migrationStore, logger } =
  runExceptT do
    migrations <- ExceptT migrationStore.loadMigrations
    ExceptT $ Right <$> (logger $ "MIGRATION IDS: " <> show (migrationIds migrations))
    currentVersion <- ExceptT versionStore.currentVersion
    ExceptT $ Right <$> (logger $ "CURRENT VERSION: " <> show currentVersion)
    void $ for (remainingMigrations currentVersion migrations) (runMigration migrator >>> ExceptT)

rollback :: forall m id a. (Ord id) => (Monad m) => Migrator m id a -> m (Either String Unit)
rollback migrator@{ versionStore, migrationStore } =
  runExceptT do
    migrations <- ExceptT migrationStore.loadMigrations
    currentVersion <- ExceptT versionStore.currentVersion
    let
      previousVersion = previousMigration currentVersion migrations
    case (previousMigration currentVersion migrations) of
      Nothing -> pure unit
      (Just migration) -> ExceptT $ runMigration migrator migration

revert :: forall a b. (Show a) => Migration a b -> a -> Migration a b
revert migration id = { id, up, down, description }
  where
  up = migration.down

  down = migration.up

  description = "REVERT: " <> show migration.id <> " (" <> migration.description <> ")"
