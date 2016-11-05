{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE RecordWildCards #-}

module Handlers where

import BasicPrelude hiding (insert)
import Control.Monad.Except
import Control.Monad.Logger
import Data.Aeson
import Data.Time.Clock
import Database.Persist.Sql
import GHC.Generics
import Safe (headMay)

import Game
import Models

type ActionM = LoggingT (ExceptT ActionErr IO)

instance MonadAction ActionM

data VisitRes
  = PickCard [ChanceCard]
  | InsufficientMoneyToBuy
  | InsufficientMoneyToRent
  | ReceivedStartBonus Money
  | IllegalVisitWhileInJail
  | PayedRent Money
              Team
              [Word]
  | TeamPutInJail UTCTime
  | RepeatedVisit
  | NoVisitResult
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
  msiteE <- getBy $ UniqueSiteToken siteT
  mteamE <- getBy $ UniqueTeamToken teamT
  case (msiteE, mteamE) of
    (Nothing, _) -> throwError $ SiteNotFound siteT
    (Just _, Nothing) -> throwError $ TeamNotFound teamT
    -- TODO: do team refresh here (maybe no longer jailed)
    (Just siteE, Just teamE) -> visit' siteE teamE
  where
    visit'
      :: MonadIO m
      => SiteE -> TeamE -> SqlPersistT m VisitRes
    visit' siteE teamE = do
      repeated <- isRepeatedVisit siteE teamE
      now <- storeVisit siteE teamE
      if repeated
        then return RepeatedVisit
        else let team = entityVal teamE
                 site = entityVal siteE
                 teamId = entityKey teamE
             in case (teamStatus team, siteSiteType site, sitePrice site) of
                  (ToJail, Jail, _) -> do
                    _ <- update teamId [TeamStatus =. InJail now]
                    return $ TeamPutInJail now
                  (ToStart bonus, Start, _) -> do
                    _ <- update teamId [TeamMoney +=. bonus, TeamStatus =. Free]
                    return $ ReceivedStartBonus bonus
                  (InJail _, Jail, _) -> return NoVisitResult
                  (InJail _, _, _) -> return IllegalVisitWhileInJail
                  (Free, _, Just price) -> do
                    mownerE <- getOwner siteE
                    case mownerE of
                      Nothing ->
                        if price <= teamMoney team
                          then do
                            chanceCards <- drawChanceCards 3 (Just siteE)
                            return $ PickCard chanceCards
                          else return InsufficientMoneyToBuy
                      Just ownerE -> do
                        (rent, dice) <- computeRent siteE
                        if rent <= teamMoney team
                          then do
                            _ <- update (entityKey ownerE) [TeamMoney +=. rent]
                            _ <- update (entityKey teamE) [TeamMoney -=. rent]
                            return $ PayedRent rent (entityVal ownerE) dice
                          else return InsufficientMoneyToRent
                  _ -> return NoVisitResult
    isRepeatedVisit
      :: MonadIO m
      => SiteE -> TeamE -> SqlPersistT m Bool
    isRepeatedVisit siteE teamE = do
      lastVisit <- getLastVisit teamE
      return $ (Just $ entityVal siteE) == lastVisit
    getLastVisit :: MonadIO m
              => TeamE -> SqlPersistT m (Maybe Site)
    getLastVisit teamE = do
      let teamId = entityKey teamE
      ordVisitsE <- selectList [VisitTeamId ==. teamId] [Desc VisitWhen, LimitTo 1]
      case ordVisitsE of
        [] -> return Nothing
        (visitE:_) -> get (visitSiteId . entityVal $ visitE)
    storeVisit
      :: MonadIO m
      => SiteE -> TeamE -> SqlPersistT m UTCTime
    storeVisit siteE teamE = do
      now <- liftIO getCurrentTime
      _ <- insert $ Visit now (entityKey siteE) (entityKey teamE)
      return now
    getOwner
      :: MonadIO m
      => SiteE -> SqlPersistT m (Maybe TeamE)
    getOwner = undefined
    drawChanceCards
      :: MonadIO m
      => Word -> Maybe SiteE -> SqlPersistT m [ChanceCard]
    drawChanceCards = undefined
    computeRent :: MonadIO m => SiteE -> SqlPersistT m (Money, [Word])
    computeRent = undefined

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
        (Just siteE, Just teamE) -> buy' siteE teamE
  where
    buy'
      :: MonadIO m
      => SiteE -> TeamE -> SqlPersistT m BuyRes
    buy' siteE teamE =
      let site = entityVal siteE
      in case sitePrice site of
           Nothing -> return CannotBuySite
           Just price ->
             let team = entityVal teamE
             in if teamMoney team <= price
                  then return InsufficientMoney
                  else do
                    let teamId = entityKey teamE
                    _ <- update teamId [TeamMoney -=. price]
                    _ <- update (entityKey siteE) [SiteOwnerId =. Just teamId]
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
  => SqlPersistT m [SiteD]
listSites = do
  sitesE <- selectList [] []
  return $ (toSiteD . entityVal) `fmap` sitesE

syncTeam
  :: MonadAction m
  => TeamToken -> Location -> SqlPersistT m (Money, TeamStatus)
syncTeam teamT loc = do
  mteamE <- getBy $ UniqueTeamToken teamT
  case mteamE of
    Nothing -> throwError $ TeamNotFound teamT
    Just teamE -> do
      -- TODO: TeamLocation needs timestamp
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
