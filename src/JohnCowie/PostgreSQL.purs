module JohnCowie.PostgreSQL where

import Prelude
import Control.Monad.Except.Trans (ExceptT, runExceptT)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Database.PostgreSQL.PG (PGError)
import Database.PostgreSQL.PG as PG
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Console as Console
import JohnCowie.PostgreSQL.URI (fromURI)

type DB = PG.Pool

withConnection :: forall a. DB -> (PG.Connection -> ExceptT PGError Aff a) -> ExceptT PGError Aff a
withConnection = PG.withConnection runExceptT

withTransaction :: forall a. PG.Connection -> ExceptT PGError Aff a -> ExceptT PGError Aff a
withTransaction = PG.withTransaction runExceptT

createConnectionPool :: PG.PoolConfiguration -> Effect DB
createConnectionPool poolConfig =
  PG.newPool
    (poolConfig { idleTimeoutMillis = Just 1000 })

toPGError :: String -> PG.PGError
toPGError s = PG.ConversionError s

runQuery :: forall a. PG.Pool -> (PG.Connection -> ExceptT PGError Aff a) -> Aff (Either PG.PGError a)
runQuery pool query =
  runExceptT do
    withConnection pool
      $ \conn -> do
          withTransaction conn $ query conn

connectionMsg :: PG.PoolConfiguration -> String
connectionMsg poolConfig = "Connected to database " <> db <> " at " <> hostAndPort
  where
  db = poolConfig.database
  host = fromMaybe "" poolConfig.host
  port = fromMaybe "" $ show <$> poolConfig.port
  hostAndPort = host <> ":" <> port

getDB :: String -> Effect (Either String PG.Pool)
getDB dbUri = case fromURI dbUri of
  (Left err) -> pure (Left err)
  (Right poolConfig) -> do
    pool <- createConnectionPool poolConfig
    Console.log $ connectionMsg poolConfig
    pure $ Right pool
