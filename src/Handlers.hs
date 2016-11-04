{-# LANGUAGE FlexibleContexts #-}
module Handlers where

import Control.Monad.Except
import Control.Monad.Trans (MonadIO, liftIO)
import Database.Persist.Sql
import Data.Time.Clock (getCurrentTime)

import Client
import Models
import qualified ToGame
import qualified ToPersist


interpret :: MonadIO m => ClientAPI Token a -> SqlPersistT m a
interpret = ToPersist.interpret . ToGame.interpret

handleListSites :: MonadIO m
               => SqlPersistT m [SiteDetails]
handleListSites = interpret listSites

handleNewTeam :: MonadIO m
              => TeamDetails -> SqlPersistT m Token
handleNewTeam = interpret . newTeam

handleNewChanceCards :: MonadIO m
                    => [CardDetails] -> SqlPersistT m [Token]
handleNewChanceCards = mapM (interpret . newChanceCard)

handleNewSites :: MonadIO m
               => [SiteDetails] -> SqlPersistT m [Token]
handleNewSites = mapM (interpret . newSite)

handleVisit :: (MonadIO m, MonadError (ClientError Token) m)
            => Token -> Token -> SqlPersistT m VisitResult
handleVisit siteTk teamTk = do
  now <- liftIO getCurrentTime
  result <- interpret (visit siteTk teamTk now)
  case result of
    Left err -> throwError err
    Right res -> return res

handleBuy :: (MonadIO m, MonadError (ClientError Token) m)
          => Token -> Token -> Token -> Maybe Int -> SqlPersistT m BuyResult
handleBuy siteTk teamTk cardTk manswer = undefined
