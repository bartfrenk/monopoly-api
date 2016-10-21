{-# LANGUAGE TypeFamilies #-}

module Store where

import Control.Monad.Trans (MonadIO)
import Control.Monad.Reader (ReaderT)
import Database.Persist.Sql

import Entities

getEntity :: (MonadIO m, PersistRecordBackend record backend)
          => Key record -> ReaderT backend m (Maybe (Entity record))
getEntity = undefined

getLastVisited :: MonadIO m => TeamId -> SqlPersistT m (Maybe (Entity Location))
getLastVisited = undefined

getOwner :: MonadIO m => LocationId -> SqlPersistT m (Entity Team)
getOwner = undefined

getLocation :: MonadIO m => LocationId -> SqlPersistT m (Maybe (Entity Location))
getLocation = getEntity

getTeam :: MonadIO m => TeamId -> SqlPersistT m (Maybe (Entity Team))
getTeam = getEntity

storeVisit :: MonadIO m => Visit -> SqlPersistT m ()
storeVisit = undefined

processTransaction :: MonadIO m => Transaction -> SqlPersistT m ()
processTransaction = undefined

