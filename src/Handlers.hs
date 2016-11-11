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
import Servant (NoContent(..))
import GHC.Generics
import Data.Maybe (fromJust)

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
              [DieResult]
  | TeamPutInJail UTCTime
  | SiteOwnedByVisitor
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
  logInfoN $ unwords ["visit", tshow siteT, tshow teamT]
  msiteE <- getBy $ UniqueSiteToken siteT
  mteamE <- getBy $ UniqueTeamToken teamT
  case (msiteE, mteamE) of
    (Nothing, _) -> throwError $ SiteNotFound siteT
    (Just _, Nothing) -> throwError $ TeamNotFound teamT
    -- TODO: do team refresh here (maybe no longer jailed)
    (Just siteE, Just teamE) -> visit' siteE teamE
  where
    visit'
      :: (MonadIO m, MonadLogger m)
      => SiteE -> TeamE -> SqlPersistT m VisitRes
    visit' siteE teamE = do
      now <- liftIO getCurrentTime
      repeated <- isRepeatedVisit siteE teamE
      storeVisit now siteE teamE
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
                    _ <- pay bonus Nothing (Just teamE) Nothing StartBonus
                    _ <- update teamId [TeamStatus =. Free]
                    return $ ReceivedStartBonus bonus
                  (InJail _, Jail, _) -> return NoVisitResult
                  (InJail _, _, _) -> return IllegalVisitWhileInJail
                  (Free, _, Just price) -> do
                    mownerE <- getOwner site
                    case mownerE of
                      Nothing ->
                        if price <= teamMoney team
                          then do
                            chanceCards <- drawChanceCards (Just siteE)
                            return $ PickCard chanceCards
                          else return InsufficientMoneyToBuy
                      Just ownerE ->
                        if ownerE /= teamE
                          then do
                            (rent, dice) <- computeRent site
                            success <-
                              pay rent (Just teamE) (Just ownerE) (Just siteE) Rent
                            if success
                              then return $ PayedRent rent (entityVal ownerE) dice
                              else return InsufficientMoneyToRent
                          else return SiteOwnedByVisitor
                  _ -> return NoVisitResult
    isRepeatedVisit
      :: (MonadIO m, MonadLogger m)
      => SiteE -> TeamE -> SqlPersistT m Bool
    isRepeatedVisit siteE teamE = do
      mlastVisit <- getLastVisit teamE
      case mlastVisit of
        Nothing -> return False
        Just lastVisit -> do
          logDebugN (tshow lastVisit)
          lastSite' <- get $ visitSiteId lastVisit
          let lastSite = fromJust lastSite' --
          logDebugN (tshow lastSite)
          let currentSite = entityVal siteE
          return $
            currentSite == lastSite && siteUpdated currentSite < visitWhen lastVisit
    getLastVisit
      :: MonadIO m
      => TeamE -> SqlPersistT m (Maybe Visit)
    getLastVisit teamE = do
      let teamId = entityKey teamE
      ordVisitsE <-
        selectList [VisitTeamId ==. teamId] [Desc VisitWhen, LimitTo 1]
      case ordVisitsE of
        [] -> return Nothing
        (visitE:_) -> return $ Just $ entityVal visitE
    storeVisit
      :: MonadIO m
      => UTCTime -> SiteE -> TeamE -> SqlPersistT m ()
    storeVisit t siteE teamE = do
      _ <- insert $ Visit t (entityKey siteE) (entityKey teamE)
      return ()
    getOwner
      :: MonadIO m
      => Site -> SqlPersistT m (Maybe TeamE)
    getOwner Site {..} =
      case siteOwnerId of
        Nothing -> return Nothing
        Just ownerId' -> do
          mteam <- get ownerId'
          return $ Entity ownerId' `fmap` mteam

buy
  :: MonadAction m
  => SiteToken -> TeamToken -> BuyPermission -> SqlPersistT m BuyRes
buy siteT teamT perm = do
  logInfoN $ unwords ["buy", tshow siteT, tshow teamT, tshow perm]
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
           Just price -> do
             success <- pay price (Just teamE) Nothing (Just siteE) Sale
             if success
               then do
                 now <- liftIO getCurrentTime
                 _ <-
                   update
                     (entityKey siteE)
                     [SiteOwnerId =. Just (entityKey teamE), SiteUpdated =. now]
                 return SuccessfullyBought
               else return InsufficientMoney

newTeam
  :: MonadAction m
  => TeamU -> SqlPersistT m TeamE
newTeam teamU = do
  logInfoN $ unwords ["newTeam", tshow teamU]
  now <- liftIO getCurrentTime
  createTeam now teamU >>= insertEntity

newSites
  :: MonadAction m
  => [SiteU] -> SqlPersistT m [SiteE]
newSites sitesU = do
  logInfoN $ unwords ["newSites", tshow $ length sitesU]
  now <- liftIO getCurrentTime
  mapM (newSite now) sitesU
  where
    newSite
      :: MonadAction m
      => UTCTime -> SiteU -> SqlPersistT m SiteE
    newSite time siteU = createSite time siteU >>= insertEntity

newQuestions
  :: MonadAction m
  => [QuestionU] -> SqlPersistT m [QuestionE]
newQuestions questionsU = do
  logInfoN $ unwords ["newQuestions", tshow $ length questionsU]
  mapM newQuestion questionsU
  where
    newQuestion
      :: MonadAction m
      => QuestionU -> SqlPersistT m QuestionE
    newQuestion questionU = createQuestion questionU >>= insertEntity

listSites
  :: MonadAction m
  => SqlPersistT m [SiteD]
listSites = do
  logInfoN $ unwords ["listSites"]
  sitesE <- selectList [] []
  return $ (toSiteD . entityVal) `fmap` sitesE

syncTeam
  :: MonadAction m
  => TeamToken -> Location -> SqlPersistT m SyncData
syncTeam teamT loc = do
  logInfoN $ unwords ["syncTeam", tshow teamT, tshow loc]
  mteamE <- getBy $ UniqueTeamToken teamT
  case mteamE of
    Nothing -> throwError $ TeamNotFound teamT
    Just teamE
     -> do
      now <- liftIO getCurrentTime
      _ <- insert $ TeamLocation now (entityKey teamE) loc
      team <- updateTeam teamE
      return
        SyncData
        { money = teamMoney team
        , status = teamStatus team
        }

goToJail ::
  MonadAction m
  => TeamToken -> SqlPersistT m NoContent
goToJail teamT = do
  logInfoN $ unwords ["goToJail", tshow teamT]
  mteamE <- getBy $ UniqueTeamToken teamT
  case mteamE of
    Nothing -> throwError $ TeamNotFound teamT
    Just teamE -> do
      update (entityKey teamE) [TeamStatus =. ToJail]
      return NoContent

goToStart ::
  MonadAction m
  => TeamToken -> Maybe Money -> SqlPersistT m NoContent
goToStart teamT mamount = do
  logInfoN $ unwords ["goToStart", tshow teamT, tshow mamount]
  mteamE <- getBy $ UniqueTeamToken teamT
  let amount = fromMaybe 0 mamount
  case mteamE of
    Nothing -> throwError $ TeamNotFound teamT
    Just teamE -> do
      update (entityKey teamE) [TeamStatus =. ToStart amount]
      return NoContent


