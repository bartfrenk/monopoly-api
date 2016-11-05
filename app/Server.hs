{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Control.Monad.Except
import Control.Monad.Logger
import Control.Monad.Trans.Control (MonadBaseControl)
import Network.Wai.Handler.Warp (run)

import Data.Text.Lazy.Encoding (encodeUtf8)
import Database.Persist.Postgresql
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack)
import qualified Data.ByteString.Lazy.Char8 as Lazy
import Servant
import Text.Printf
import BasicPrelude hiding (encodeUtf8)

import API
import Models
import Game
import Actions

main :: IO ()
main = do
  migratePostgreSql
  printf "Monopoly server running on port %d\n" tcpPort
  run tcpPort $ serve api (server postgres)

postgres :: ByteString
postgres =
  "host=localhost \
                \port=8001 \
                \user=monopoly \
                \password=monopoly \
                \dbname=monopoly"

type HandlerM = SqlPersistT ActionM

toHandler :: ByteString -> LogLevel -> HandlerM a -> Handler a
toHandler connStr minLvl = translateErr . runLogging . runDb
  where
    translateErr = withExceptT toServantErr
    filterLogs = filterLogger (\_ lvl -> lvl >= minLvl)
    toServantErr e =
      err404
      { errBody = Lazy.pack $ show e
      }
    runLogging = runStdoutLoggingT . filterLogs
    runDb = withPostgresqlPool connStr 10 . runSqlPool

server :: ByteString -> Server MonopolyAPI
server connStr = enter nat $ siteServer :<|> teamServer :<|> questionServer
  where
    nat = Nat $ toHandler connStr LevelDebug
    siteServer = listSites :<|> newSites :<|> visit :<|> buy
    teamServer = newTeam :<|> syncTeam
    questionServer = newQuestions

tcpPort :: Int
tcpPort = 8000

migratePostgreSql :: IO ()
migratePostgreSql =
  runStderrLoggingT $
  withPostgresqlPool postgres 10 $
  \pool -> liftIO $ runSqlPersistMPool (runMigration migrateAll) pool
