{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Game where

import Control.Monad.Trans (MonadIO, liftIO)
import Data.Maybe (fromJust)
import Data.Aeson
import Data.Time.Clock
import Database.Persist.Sql
import GHC.Generics
import Models
import System.Random (randomIO)

data BuyPermission
  = QuestionAnswer QuestionToken
                   AnswerIndex
  | NoQuestionToken Token
  deriving (Eq, Show, Generic)

instance FromJSON BuyPermission

canBuy
  :: MonadIO m
  => BuyPermission -> SqlPersistT m Bool
canBuy (NoQuestionToken _) = return True -- check if token was dispensed?
canBuy (QuestionAnswer questionT given) = do
  mquestionE <- getBy $ UniqueQuestionToken questionT
  case mquestionE of
    Nothing -> return False
    Just questionE ->
      let q = entityVal questionE
      in return (questionAnswerIndex q == given)

refreshStatus :: UTCTime -> Team -> Team
refreshStatus now team@Team {..} =
  case teamStatus of
    InJail start ->
      if diffUTCTime now start > jailTime
        then team
             { teamStatus = Free
             }
        else team
    _ -> team

jailTime :: NominalDiffTime
jailTime = 1000

startingMoney :: Money
startingMoney = 1000

createTeam
  :: MonadIO m
  => TeamU -> m Team
createTeam TeamU {..} = do
  t <- liftIO randomIO
  return
    Team
    { teamName = name
    , teamToken = t
    , teamMoney = startingMoney
    , teamStatus = Free
    }

createSite
  :: MonadIO m
  => SiteU -> m Site
createSite SiteU {..} = do
  t <- liftIO randomIO
  return
    Site
    { siteName = name
    , siteToken = t
    , siteLocation = location
    , siteType = siteType_
    , siteColor = color
    , sitePrice = price
    , siteOwnerId = Nothing
    }

createQuestion
  :: MonadIO m
  => QuestionU -> SqlPersistT m Question
createQuestion QuestionU {..} = do
  siteId <-
    case site of
      Nothing -> return Nothing
      Just siteName -> do
        siteE <- getBy $ UniqueSiteName siteName
        return $ Just . entityKey . fromJust $ siteE
  t <- liftIO randomIO
  return
    Question
    { questionPhrase = phrase
    , questionToken = t
    , questionSiteId = siteId
    , questionOptions = options
    , questionAnswerIndex = answerIndex
    }
