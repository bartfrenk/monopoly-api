{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE RecordWildCards #-}

module Actions where

import BasicPrelude hiding (insert)
import Control.Monad.Except
import Control.Monad.Logger
import Data.Aeson
import Data.Time.Clock
import Database.Persist.Sql
import GHC.Generics

import Game
import Models

type ActionM = LoggingT (ExceptT ActionErr IO)

instance MonadAction ActionM

data VisitRes
  = PickCard [ChanceCard]
  | InsufficientMoneyToBuy
  | InsufficientMoneyToRent
  | ReceivedStartBonus
  | PayedRent Money
              Team
              [Word]
  | TeamPutInJail UTCTime
  | RepeatedVisit
  deriving (Eq, Show, Generic)

instance ToJSON VisitRes

data BuyRes
  = SuccessfullyBought
  | InsufficientMoney
  | WrongAnswerGiven
  | CannotBuySite
  deriving (Eq, Show, Generic)

instance ToJSON BuyRes

data ActionErr
  = TeamNotFound Token
  | SiteNotFound Token
  | QuestionNotFound Token
  deriving (Eq, Show)

class (MonadIO m, MonadError ActionErr m, MonadLogger m) =>
      MonadAction m

visit
  :: MonadAction m
  => SiteToken -> TeamToken -> SqlPersistT m VisitRes
visit siteT teamT = do
  logDebugN $ unwords [tshow siteT, tshow teamT]
  return RepeatedVisit

buy
  :: MonadAction m
  => SiteToken -> TeamToken -> BuyPermission -> SqlPersistT m BuyRes
buy siteT teamT perm = do
  logDebugN $ unwords [tshow siteT, tshow teamT, tshow perm]
  allowed <- canBuy perm
  if not allowed
    then return WrongAnswerGiven
    else do
      msiteE <- getBy $ UniqueSiteToken siteT
      mteamE <- getBy $ UniqueTeamToken teamT
      case (msiteE, mteamE) of
        (Nothing, _) -> throwError $ SiteNotFound siteT
        (Just _, Nothing) -> throwError $ TeamNotFound teamT
        (Just siteE, Just teamE) ->
          let site = entityVal siteE
          in buy' site teamE
  where
    buy'
      :: MonadIO m
      => Site -> TeamE -> SqlPersistT m BuyRes
    buy' Site {..} teamE =
      case sitePrice of
        Nothing -> return CannotBuySite
        Just price ->
          let team = entityVal teamE
          in if teamMoney team <= price
               then return InsufficientMoney
               else do
                 let teamId = entityKey teamE
                 let teamAfter =
                       team
                       { teamMoney = teamMoney team - price
                       }
                 _ <- replace teamId teamAfter
                 return SuccessfullyBought

newTeam
  :: MonadAction m
  => TeamU -> SqlPersistT m TeamE
newTeam teamU = createTeam teamU >>= insertEntity

newSites
  :: MonadAction m
  => [SiteU] -> SqlPersistT m [SiteE]
newSites = mapM newSite
  where
    newSite
      :: MonadAction m
      => SiteU -> SqlPersistT m SiteE
    newSite siteU = createSite siteU >>= insertEntity

newQuestions
  :: MonadAction m
  => [QuestionU] -> SqlPersistT m [QuestionE]
newQuestions = mapM newQuestion
  where
    newQuestion
      :: MonadAction m
      => QuestionU -> SqlPersistT m QuestionE
    newQuestion questionU = createQuestion questionU >>= insertEntity

listSites
  :: MonadAction m
  => SqlPersistT m [SiteE]
listSites = selectList [] []

syncTeam
  :: MonadAction m
  => TeamToken -> Location -> SqlPersistT m (Money, TeamStatus)
syncTeam teamT loc = do
  mteamE <- getBy $ UniqueTeamToken teamT
  case mteamE of
    Nothing -> throwError $ TeamNotFound teamT
    Just teamE -> do
      _ <- insert $ TeamLocation (entityKey teamE) loc
      team <- updateTeam teamE
      return (teamMoney team, teamStatus team)

updateTeam
  :: MonadAction m
  => TeamE -> SqlPersistT m Team
updateTeam teamE = do
  let team = entityVal teamE
  now <- liftIO getCurrentTime
  let team' = refreshStatus now team
  when (team' /= team) $ replace (entityKey teamE) team
  return team'
