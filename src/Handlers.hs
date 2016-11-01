{-# LANGUAGE FlexibleContexts #-}
module Handlers where

import Control.Monad.Except
import Control.Monad.Trans (MonadIO, liftIO)
import Database.Persist.Sql
import Data.Time.Clock (getCurrentTime)

import Client
import Models
import ToGame
import ToPersist

handleNewTeam :: MonadIO m
              => TeamDetails -> SqlPersistT m Token
handleNewTeam = undefined

handleVisit :: (MonadIO m, MonadError (ClientError Token) m)
            => Token -> Token -> SqlPersistT m VisitResult
handleVisit siteTk teamTk = do
  now <- liftIO getCurrentTime
  result <- toPersist $ toGame (visit siteTk teamTk now)
  case result of
    Left err -> throwError err
    Right res -> return res

