{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Game where

import BasicPrelude hiding (insert, intercalate)
import Control.Monad.Logger
import Control.Monad (when)
import Control.Monad.Trans (MonadIO, liftIO)
import Control.Monad.State
import qualified Control.Monad.State as State
import Data.Aeson
import Data.Maybe (fromJust)
import Data.Ratio
import Data.HashMap.Strict (HashMap)
import Data.List (splitAt)
import Data.Time.Clock
import Database.Persist.Sql
import GHC.Generics
import Models
import System.Random (randomIO, randomRIO)

data TeamOverview = TeamOverview
  { lastLocation :: Maybe Location
  , money :: Money
  , status :: TeamStatus
  , sitesOwned :: [String]
  , name :: String
  } deriving (Eq, Show, Generic)

instance ToJSON TeamOverview

type GameOverview = HashMap String TeamOverview

data BuyPermission
  = QuestionAnswer QuestionToken
                   AnswerIndex
  | NoQuestionToken Token
  deriving (Eq, Show, Read, Generic)

instance FromJSON BuyPermission where
  parseJSON v = read <$> parseJSON v

instance ToJSON BuyPermission where
  toJSON = toJSON . show

pay
  :: MonadIO m
  => Money
  -> Maybe TeamE
  -> Maybe TeamE
  -> Maybe SiteE
  -> TransactionReason
  -> SqlPersistT m Bool
pay amount msrcE mdestE msiteE reason = do
  now <- liftIO getCurrentTime
  case (msrcE, mdestE) of
    (Nothing, Just destE) -> do
      _ <- insert $ mkTrans now amount msrcE mdestE msiteE reason
      update (entityKey destE) [TeamMoney +=. amount]
      return True
    (Just srcE, destAccount) ->
      if teamMoney (entityVal srcE) < amount
        then return False
        else do
          _ <- insert $ mkTrans now amount msrcE mdestE msiteE reason
          case destAccount of
            Nothing -> do
              update (entityKey srcE) [TeamMoney -=. amount]
              return True
            Just destE -> do
              update (entityKey srcE) [TeamMoney -=. amount]
              update (entityKey destE) [TeamMoney +=. amount]
              return True
    _ -> error "bank to bank transaction does not make sense"

mkTrans
  :: UTCTime
  -> Money
  -> Maybe TeamE
  -> Maybe TeamE
  -> Maybe SiteE
  -> TransactionReason
  -> Transaction
mkTrans time amount msrcE mdestE msiteE reason =
  let msrcId = entityKey <$> msrcE
      mdestId = entityKey <$> mdestE
      msiteId = entityKey <$> msiteE
  in Transaction time amount msrcId mdestId msiteId reason

-- REVIEW: candidate for refactoring
computeRent
  :: (MonadIO m, MonadLogger m)
  => Site -> SqlPersistT m (Money, [DieResult])
computeRent Site {..} = do
  let ownerId = fromJust siteOwnerId
  let price = fromJust sitePrice
  case siteSiteType of
    Street -> do
      (owned, total) <- getTotalStreetOwned ownerId siteColor
      logDebugN $
        unwords [tshow siteSiteType, "owned", tshow owned, "total", tshow total]
      return (computeStreetRent price owned total, [])
    Utility _ -> do
      owned <- getTotalUtilityOwned ownerId
      dieResult <- liftIO $ randomRIO (1, 6)
      logDebugN $
        unwords
          [tshow siteSiteType, "owned", tshow owned, "die", tshow dieResult]
      return (computeUtilityRent price owned dieResult, [dieResult, price, owned])
    Station -> do
      owned <-
        selectList [SiteOwnerId ==. Just ownerId, SiteSiteType ==. Station] []
      logDebugN $ unwords [tshow siteSiteType, "owned", tshow owned]
      return (computeStationRent price (length owned), [])
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
    computeStreetRent price total owned =
      case (total, owned) of
        (_, 0) -> 0
        (1, _) -> floor (toRational price / 2)
        (2, 3) -> price
        _ -> 2 * price
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
    computeUtilityRent :: Money -> Int -> Int -> Money
    computeUtilityRent price owned dieResult =
      case owned of
        0 -> 0
        1 -> floor $ toRational (4 * dieResult * price) / 10
        _ -> dieResult * price
    computeStationRent :: Money -> Int -> Money
    computeStationRent price owned = floor $ toRational (price * owned) / 2

drawChanceCards
  :: MonadIO m
  => Maybe SiteE -> SqlPersistT m [ChanceCard]
drawChanceCards msite = do
  siteSpecific <- getSiteSpecificCards msite
  nonSpecific <- getNonSpecificCards
  liftIO $ execStateT (draw siteSpecific nonSpecific) []
  where
    getSiteSpecificCards
      :: MonadIO m
      => Maybe SiteE -> SqlPersistT m [ChanceCard]
    getSiteSpecificCards msiteE =
      case msiteE of
        Nothing -> return []
        Just siteE -> do
          let siteId = entityKey siteE
          questions <- selectList [QuestionSiteId ==. Just siteId] []
          return $ (Q . toQuestionD . entityVal) `fmap` questions
    getNonSpecificCards
      :: MonadIO m
      => SqlPersistT m [ChanceCard]
    getNonSpecificCards = do
      questions <- selectList [QuestionSiteId ==. Nothing] []
      return $ (Q . toQuestionD . entityVal) `fmap` questions

draw :: [ChanceCard] -> [ChanceCard] -> StateT [ChanceCard] IO ()
draw must fill =
  let limit = 3
      jailProb :: Rational
      jailProb = 1 % 10
      toStartProb :: Rational
      toStartProb = 1 % 10
      startBonus = 100
  in do unless (null must) $
          do siteCards <- liftIO $ choice 1 must
             modify' (++ siteCards)
        hasJailCard <- liftIO $ bernoulli jailProb
        when hasJailCard $ modify' (++ [GoToJail])
        hasToStartCard <- liftIO $ bernoulli toStartProb
        when hasToStartCard $ modify' (++ [GoToStart startBonus])
        currentCount <- length `fmap` State.get
        genericCards <- liftIO $ choice (limit - currentCount) fill
        modify (++ genericCards)

bernoulli :: Rational -> IO Bool
bernoulli r = do
  n <- randomRIO (1, denominator r)
  return $ n <= numerator r

choice :: Int -> [a] -> IO [a]
choice n = choiceAcc n []
  where
    choiceAcc :: Int -> [a] -> [a] -> IO [a]
    choiceAcc cnt drawn left =
      if cnt == 0 || null left
        then return drawn
        else do
          i <- randomRIO (0, length left - 1)
          choiceAcc (cnt - 1) ((left !! i) : drawn) (dropAt i left)

dropAt :: Int -> [a] -> [a]
dropAt i xs =
  let (before, after) = splitAt i xs
  in before ++ tail after

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
jailTime = 300

startingMoney :: Money
startingMoney = 1000

createTeam
  :: MonadIO m
  => UTCTime -> TeamU -> m Team
createTeam time TeamU {..} = do
  tk <- liftIO randomIO
  return
    Team
    { teamName = name
    , teamToken = tk
    , teamMoney = startingMoney
    , teamStatus = Free
    , teamStatusUpdated = time
    }

createSite
  :: MonadIO m
  => UTCTime -> SiteU -> m Site
createSite time SiteU {..} = do
  tk <- liftIO randomIO
  return
    Site
    { siteName = name
    , siteToken = tk
    , siteLocation = location
    , siteSiteType = siteType
    , siteColor = color
    , sitePrice = price
    , siteOwnerId = Nothing
    , siteUpdated = time
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

updateTeam
  :: MonadIO m
  => TeamE -> SqlPersistT m Team
updateTeam teamE = do
  let team = entityVal teamE
  now <- liftIO getCurrentTime
  let team' = refreshStatus now team
  when (team' /= team) $ replace (entityKey teamE) team'
  return team'

getLastLocation
  :: MonadIO m
  => Key Team -> SqlPersistT m (Maybe Location)
getLastLocation teamK = do
  mloc <-
    selectFirst [TeamLocationTeamId ==. teamK] [Desc TeamLocationWhen]
  return $ (teamLocationLocation . entityVal) `fmap` mloc

getTotalSitesOwned
  :: MonadIO m
  => Key Team -> SqlPersistT m [String]
getTotalSitesOwned teamK = do
  owned <- selectList [SiteOwnerId ==. Just teamK] []
  return $ (siteName . entityVal) `fmap` owned
