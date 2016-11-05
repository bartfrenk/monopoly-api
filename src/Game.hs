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

-- REVIEW: candidate for refactoring
computeRent
  :: MonadIO m
  => Site -> SqlPersistT m (Money, [DieResult])
computeRent Site {..} = do
  let ownerId = fromJust siteOwnerId
  case siteSiteType of
    Street -> do
      (owned, total) <- getTotalStreetOwned ownerId siteColor
      let price = fromJust sitePrice
      return (computeStreetRent price owned total, [])
    Utility _ -> do
      owned <- getTotalUtilityOwned ownerId
      let dieResult = 3
      return (computeUtilityRent owned dieResult, [dieResult])
    Station -> do
      owned <-
        selectList [SiteOwnerId ==. Just ownerId, SiteSiteType ==. Station] []
      return (computeStationRent (length owned), [])
    _ -> return (0, [])
  where
    getTotalStreetOwned
      :: MonadIO m
      => TeamId -> Color -> SqlPersistT m (Int, Int)
    getTotalStreetOwned teamId color = do
      sameColor <- selectList [SiteColor ==. color] []
      owned <- selectList [SiteColor ==. color, SiteOwnerId ==. Just teamId] []
      return (length sameColor, length owned)
    computeStreetRent :: Money -> Int -> Int -> Money
    computeStreetRent price total owned -- TODO: what should this be?
     = floor (toRational (price * owned) / toRational total)
    getTotalUtilityOwned
      :: MonadIO m
      => TeamId -> SqlPersistT m Int
    getTotalUtilityOwned teamId = do
      owned <-
        selectList
          ([SiteSiteType ==. Utility Water, SiteOwnerId ==. Just teamId] ||.
           [SiteSiteType ==. Utility Electra, SiteOwnerId ==. Just teamId])
          []
      return $ length owned
    computeUtilityRent :: Int -> Int -> Money
    computeUtilityRent owned dieResult =
      case owned of
        0 -> 0
        1 -> 4 * dieResult
        _ -> 10 * dieResult
    computeStationRent :: Int -> Money
    computeStationRent _ = 12 -- TODO: what should this be?

drawChanceCards
  :: MonadIO m
  => Int -> Maybe SiteE -> SqlPersistT m [ChanceCard]
drawChanceCards limit _ = do
  cardsE <- selectList [] [LimitTo limit]
  return $ (Q . toQuestionD . entityVal) `fmap` cardsE


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
    , siteSiteType = siteType
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
