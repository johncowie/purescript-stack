module Server.DB where

import CustomPrelude
import Control.Monad.Except.Trans (ExceptT(..), runExceptT)
import Data.Array (head)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\))
import Data.Either (Either(..))
import Data.Newtype (unwrap, wrap)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Console as Console
import Data.Argonaut.Core (Json)

import Database.PostgreSQL.PG as PG
import Database.PostgreSQL.Row (Row0(Row0), Row1(Row1), Row2(Row2))

import Server.DBConnection (fromURI)
import Server.Domain (AppName, EventId, NewUser, User, UserId)

type PG a = ExceptT PG.PGError Aff a
type Pool = PG.Pool

withConnection :: forall a. PG.Pool -> (PG.Connection -> PG a) -> PG a
withConnection = PG.withConnection runExceptT

withTransaction :: forall a. PG.Connection -> PG a -> PG a
withTransaction = PG.withTransaction runExceptT

createConnectionPool :: PG.PoolConfiguration -> Effect PG.Pool
createConnectionPool poolConfig = PG.newPool
  (poolConfig { idleTimeoutMillis = Just 1000 })

runQuery :: forall a. PG.Pool -> (PG.Connection -> PG a) -> Aff (Either PG.PGError a)
runQuery pool query = runExceptT do
  withConnection pool $ \conn -> do
    withTransaction conn $ query conn

addEvent :: UserId -> AppName -> Json -> PG.Pool -> Aff (Either PG.PGError Int)
addEvent userId app event pool = runQuery pool \conn -> do
  rows <- PG.query conn (PG.Query """
    INSERT INTO EVENTS (id, app, event, user_id)
    VALUES (
	      (SELECT COALESCE(MAX(id), 0) + 1 FROM events)
      , $1
      , $2
      , $3
    )
    RETURNING ID;
  """) (unwrap app /\ event /\ unwrap userId)
  pure $ fromMaybe 0 $ head $ map (\(Row1 id) -> id) rows

retrieveEvents :: UserId -> AppName -> Maybe EventId -> PG.Pool -> Aff (Either PG.PGError (Array (Tuple Int Json)))
retrieveEvents userId app eventIdM pool = runQuery pool \conn -> do
  PG.query conn (PG.Query """
    SELECT id, event FROM events
    WHERE app = $1
    AND user_id = $2
    AND id > $3
    ORDER BY id desc;
    """) (unwrap app /\ unwrap userId /\ maybe 0 unwrap eventIdM)

retrieveLatestSnapshot :: UserId -> AppName -> PG.Pool -> Aff (Either PG.PGError (Maybe (Tuple Json Int)))
retrieveLatestSnapshot userId app pool = runQuery pool \conn -> do
  rows <- PG.query conn (PG.Query """
    SELECT snapshot, up_to_event
    FROM snapshots
    WHERE app = $1
    AND user_id = $2
    ORDER BY up_to_event DESC
    LIMIT 1;
  """) (unwrap app /\ unwrap userId)
  pure $ head rows

insertSnapshot :: UserId -> AppName -> Json -> Int -> PG.Pool -> Aff (Either PG.PGError Unit)
insertSnapshot userId app snapshot upToEvent pool = runQuery pool \conn -> do
  PG.execute conn (PG.Query """
    INSERT INTO snapshots (id, app, user_id, snapshot, up_to_event)
    VALUES (
      (select coalesce(max(id), 0) + 1 from snapshots)
    , $1
    , $2
    , $3
    , $4
    );
  """) (unwrap app /\ unwrap userId /\ snapshot /\ upToEvent)

upsertUser :: NewUser -> PG.Pool -> Aff (Either PG.PGError UserId)
upsertUser user pool = runQuery pool \conn -> do
  rows <- PG.query conn (PG.Query """
    INSERT INTO users (third_party, third_party_id, name) VALUES ($1, $2, $3)
    ON CONFLICT ON CONSTRAINT users_third_party_third_party_id_key
    DO UPDATE SET name = $3
    RETURNING id;
  """) (show user.thirdParty /\ user.thirdPartyId /\ user.name)
  case rows of
    [(Row1 id)] -> pure $ wrap id
    _ -> ExceptT $ pure $ Left $ PG.ConversionError "No ID returned"

retrieveUsers :: PG.Pool -> Aff (Either PG.PGError (Array User))
retrieveUsers pool = runQuery pool \conn -> do
  rows <- PG.query conn (PG.Query """
    SELECT id, name FROM users;
  """) Row0
  pure $ map (\(Row2 id name) -> {id: wrap id, name}) rows

connectionMsg :: PG.PoolConfiguration -> String
connectionMsg poolConfig = "Connected to database " <> db <> " at " <> hostAndPort
  where db = poolConfig.database
        host = fromMaybe "" poolConfig.host
        port = fromMaybe "" $ show <$> poolConfig.port
        hostAndPort = host <> ":" <> port

showDBError :: forall a. Either PG.PGError a -> Either String a
showDBError (Left err) = Left (show err)
showDBError (Right a) = Right a

getDB :: String -> Effect (Either String PG.Pool)
getDB dbUri = case fromURI dbUri of
  (Left err) -> pure (Left err)
  (Right poolConfig) -> do
    pool <- createConnectionPool poolConfig
    Console.log $ connectionMsg poolConfig
    pure $ Right pool
