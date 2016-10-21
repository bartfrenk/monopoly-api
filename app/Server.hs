{-# LANGUAGE FlexibleContexts #-}

module Main where

import Control.Monad.Except
import Control.Monad.Logger
import Control.Monad.Trans.Control (MonadBaseControl)
import Network.Wai.Handler.Warp (run)

import Database.Persist.Postgresql
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack)
import Servant

import API (api, MonopolyAPI)
import Entities (migrateAll)
import Types
import Handlers
import Entities

main :: IO ()
main = do
  migratePostgreSql
  putStrLn "Monopoly server"
  run tcpPort $ serve api (server postgres)

postgres :: ByteString
postgres =
  pack
    "host=localhost \
                \port=5432 \
                \user=monopoly \
                \password=monopoly \
                \dbname=monopoly"

type HandlerM = SqlPersistT (LoggingT (ExceptT ServantErr IO))

server :: ByteString -> Server MonopolyAPI
server connStr = enter (Nat f) (registerTeam
                           :<|> getAllLocations
                           :<|> registerLocations
                           :<|> handleVisit)
  where
    filt = filterLogger (\_ lvl -> lvl > LevelDebug)
    f :: SqlPersistT (LoggingT (ExceptT ServantErr IO)) a -> ExceptT ServantErr IO a
    f = runStdoutLoggingT . filt . runPostgreSql connStr 10

tcpPort :: Int
tcpPort = 8000

runPostgreSql
  :: (MonadIO m, MonadBaseControl IO m, MonadLogger m)
  => ByteString -> Int -> SqlPersistT m a -> m a
runPostgreSql connStr pools = withPostgresqlPool connStr pools . runSqlPool

migratePostgreSql :: IO ()
migratePostgreSql =
  runStderrLoggingT $
  withPostgresqlPool postgres 10 $
  \pool -> liftIO $ runSqlPersistMPool (runMigration migrateAll) pool
