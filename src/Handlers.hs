{-# LANGUAGE OverloadedStrings #-}

module Handlers (module Handlers) where

import Control.Monad.Logger (MonadLogger, logInfoN)
import Control.Monad.Trans (MonadIO, liftIO)
import Data.Text (pack)
import Database.Persist.Sql (SqlPersistT,
                             insert,
                             selectList,
                             entityVal)
import Data.Time.Clock


import Entities
import Monopoly
import Store
import Types

registerTeam :: (MonadLogger m, MonadIO m)
           => TeamDetails -> SqlPersistT m TeamId
registerTeam details = do
  let team = createTeam details
  teamId <- insert team
  logInfoN $ pack $ "Created team " ++ show team
  return teamId

registerLocations :: (MonadLogger m, MonadIO m)
                => [Location] -> SqlPersistT m [LocationId]
registerLocations locs = do
  locIds <- mapM insert locs
  logInfoN $ pack $ "Created " ++ show (length locIds) ++ " locations"
  return locIds

getAllLocations :: MonadIO m => SqlPersistT m [Location]
getAllLocations = do
  locs <- selectList [] []
  return $ entityVal `fmap` locs

handleVisit :: (MonadLogger m, MonadIO m)
            => LocationId -> TeamId -> SqlPersistT m VisitResult
handleVisit locId teamId = do
  lastVisited <- getLastVisited teamId
  owner <- getOwner locId
  loc <- getLocation locId
  visitor <- getTeam teamId
  visit <- liftIO $ createVisit loc visitor <$> getCurrentTime

  logInfoN $ pack $ "Created visit " ++ show visit
  storeVisit visit

  let visitResult = createVisitResult owner loc visitor lastVisited
  let transactions = createTransactions visitResult

  mapM_ processTransaction transactions
  return visitResult
