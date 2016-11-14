{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Control.Monad.Except
import Control.Monad.Logger
import Network.Wai.Handler.Warp (run)

import Database.Persist.Postgresql
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy.Char8 as Lazy
import Servant
import Text.Printf
import BasicPrelude hiding (encodeUtf8)
import Network.Wai.Middleware.Cors
import qualified Network.Wai as Wai

import API
import Models
import Handlers

main :: IO ()
main = do
  migratePostgreSql
  printf "Monopoly server running on port %d\n" tcpPort
  run tcpPort $ customCors $ serve api (server postgres)

customCors :: Wai.Middleware
customCors = cors (const $ Just customCorsResourcePolicy)
  where
    customCorsResourcePolicy =
      simpleCorsResourcePolicy
      { corsRequestHeaders = simpleHeaders
      }

postgres :: ByteString
postgres =
  "host=store \
                \port=5432 \
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
    runLogging = runStderrLoggingT . filterLogs
    runDb = withPostgresqlPool connStr 10 . runSqlPool

server :: ByteString -> Server MonopolyAPI
server connStr = enter nat $ siteServer :<|> teamServer :<|> questionServer
  where
    nat = Nat $ toHandler connStr LevelDebug
    siteServer =
      listSites :<|> succeed :<|> -- hack to get OPTIONS endpoints!
      newSites :<|>
      succeed :<|>
      visit :<|>
      visitOption :<|>
      buy :<|>
      buyOption
    teamServer =
      newTeam :<|> succeed :<|> syncTeam :<|> syncOption :<|> goToJail :<|> goToJailOption :<|>
      goToStart :<|>
      goToStartOption :<|>
      gameOverview :<|>
      gameOverviewOption
    questionServer = newQuestions :<|> succeed

tcpPort :: Int
tcpPort = 8000

migratePostgreSql :: IO ()
migratePostgreSql =
  runStderrLoggingT $
  withPostgresqlPool postgres 10 $
  \pool -> liftIO $ runSqlPersistMPool (runMigration migrateAll) pool

succeed :: HandlerM NoContent
succeed = return NoContent

gameOverviewOption :: HandlerM NoContent
gameOverviewOption = return NoContent

visitOption :: SiteToken -> TeamToken -> HandlerM NoContent
visitOption _ _ = return NoContent

buyOption :: SiteToken -> TeamToken -> HandlerM NoContent
buyOption _ _ = return NoContent

syncOption :: TeamToken -> HandlerM NoContent
syncOption _ = return NoContent

goToJailOption :: TeamToken -> HandlerM NoContent
goToJailOption _ = return NoContent

goToStartOption :: TeamToken -> HandlerM NoContent
goToStartOption _ = return NoContent
