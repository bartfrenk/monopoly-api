{-# LANGUAGE OverloadedStrings #-}

module Handlers (module Handlers) where

import Control.Monad.Logger (MonadLogger, logInfoN)
import Control.Monad.Trans (MonadIO)
import Data.Text (pack)
import Database.Persist.Sql (SqlPersistT,
                             insert,
                             selectList,
                             entityVal)

import Entities
import Store

createTeam :: (MonadLogger m, MonadIO m)
           => Team -> SqlPersistT m TeamId
createTeam team = do
  id <- insert team
  logInfoN $ pack $ "Created team " ++ show team
  return id

createLocations :: (MonadLogger m, MonadIO m)
                => [Location] -> SqlPersistT m [LocationId]
createLocations locs = do
  ids <- mapM insert locs
  logInfoN $ pack $ "Created " ++ show (length ids) ++ " locations"
  return ids

getAllLocations :: MonadIO m => SqlPersistT m [Location]
getAllLocations = do
  locs <- selectList [] []
  return $ entityVal `fmap` locs
