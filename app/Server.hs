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
import Text.Printf

import API
import Models
import Handlers

main :: IO ()
main = do
  migratePostgreSql
  printf "Monopoly server running on port %d\n" tcpPort
  run tcpPort $ serve api (server postgres)

siteAPI :: Proxy SiteAPI
siteAPI = Proxy

postgres :: ByteString
postgres =
  pack
    "host=localhost \
                \port=8001 \
                \user=monopoly \
                \password=monopoly \
                \dbname=monopoly"

type HandlerM = SqlPersistT (LoggingT (ExceptT (ClientError Token) IO))

-- TODO: more specific
toServantErr :: ClientError Token -> ServantErr
toServantErr _ = err404

-- TODO: refactor
server :: ByteString -> Server MonopolyAPI
server connStr = siteServer :<|> teamServer :<|> cardServer
  where
    siteServer = enter (Nat f) $
      handleListSites :<|>
      handleNewSites :<|>
      handleVisit :<|>
      handleBuy
    teamServer = enter (Nat f) handleNewTeam
    cardServer :: [CardDetails] -> Handler [Token]
    cardServer = enter (Nat f) handleNewChanceCards
    filt = filterLogger (\_ lvl -> lvl > LevelDebug)
    f :: HandlerM a -> Handler a
    f =  withExceptT toServantErr . runStdoutLoggingT . filt . runPostgreSql connStr 10

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
