{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DuplicateRecordFields #-}
module ToPersist where

import Control.Exception
import Control.Monad.Free (foldFree)
import Control.Monad.Trans (MonadIO, liftIO)
import Crypto.Random
import Data.Maybe
import Data.Time.Clock
import Data.Typeable


import Database.Persist.Sql

import Game
import Models

data InvalidState = InvalidState String deriving (Typeable, Show)

instance Exception InvalidState

interpret :: MonadIO m => GameAPI Token a -> SqlPersistT m a
interpret = foldFree interpret'

interpret' :: MonadIO m => GameF Token a -> SqlPersistT m a

interpret' (GetRent site cont) = undefined

interpret' (Transfer amount src dest cont) =
  case (src, dest) of
    (Bank, Bank) -> return $ cont True
    (Bank, TeamAcc team) -> do
      updateTeam $ team { teamWallet = teamWallet team + amount }
      return $ cont True
    (TeamAcc src', Bank) ->
      if amount > teamWallet src'
      then return $ cont False
      else do
        updateTeam $ src' { teamWallet = teamWallet src' - amount }
        return $ cont True
    (TeamAcc src', TeamAcc dest') ->
      if amount > teamWallet src'
      then return $ cont False
      else do
        updateTeam $ src' { teamWallet = teamWallet src' - amount }
        updateTeam $ dest' { teamWallet = teamWallet dest' + amount }
        return $ cont True


interpret' (CreateSite SiteDetails{..} cont) = do
  token <- liftIO createToken
  let site = Site {
        siteName = name,
        siteToken = token,
        siteLocation = location,
        siteType = sitetype,
        siteColor = color,
        sitePrice = price,
        siteOwner = Nothing
        }
  _ <- insert site
  return $ cont token

interpret' (CreateChanceCard CardDetails{..} cont) = do
  token <- liftIO createToken
  msiteTk <- -- throw when name does not match
    case cardSiteName of
      Nothing -> return Nothing
      Just n -> do
        s <- getBy $ UniqueSiteName n
        return $ Just . siteToken . entityVal . fromJust $ s

  let card = ChanceCard {
        chanceCardToken = token,
        chanceCardQuestion = question,
        chanceCardOptions = options,
        chanceCardSiteToken = msiteTk,
        chanceCardAnswer = answer
        }
  _ <- insert card
  return $ cont token

interpret' (GetSites cont) = do
  sites <- selectList [] []
  return $ cont $ (getDetails . entityVal) `map` sites
    where
      getDetails Site{..} = SiteDetails {
        name = siteName,
        location = siteLocation,
        sitetype = siteType,
        color = siteColor,
        price = sitePrice
        }

interpret' (DrawChanceCards count cont) = undefined

interpret' (IsRepeatedVisit site team cont) = undefined

-- interpret' (Transfer amount src dest cont) =
--   case (src, dest) of
--     (Bank, Bank) -> return $ cont True
--     (Bank, TeamAcc dest') -> updateTeam { dest' { teamWallet =

-- TODO: handle duplicate token
interpret' (CreateTeam details funds cont) = do
  token <- liftIO createToken
  let team = Team (name (details :: TeamDetails)) token funds Free
  _ <- insert team
  return $ cont token

interpret' (PayStartBonus team cont) = do
  let team' =
        case teamStatus team of
          ToStart bonus -> team { teamWallet = teamWallet team + bonus,
                                  teamStatus = Free }
          _ -> team
  updateTeam team'
  return cont

interpret' (PutInJail team cont) = do
  t <- liftIO getCurrentTime
  updateTeam $ team { teamStatus = InJail t }
  return cont

interpret' (FreeFromJail team cont) = do
  let team' = team { teamStatus = Free }
  updateTeam team'
  return $ cont team'

interpret' (MakeOwner site team cont) = do
  updateSite $ site { siteOwner = Just $ teamToken team }
  return cont

interpret' (GetOwner site cont) = do
  owner' <- getOwnerFromStore site
  return $ cont owner'

createToken :: IO Token
createToken = do
  g <- newGenIO :: IO SystemRandom
  let (bs, _) = throwLeft $ genBytes 16 g
  return bs

updateTeam :: MonadIO m => Team -> SqlPersistT m ()
updateTeam team = do
  teamE' <- getBy $ UniqueTeamToken $ teamToken team
  case teamE' of
    Nothing ->
      throw $ InvalidState $ "Team " ++ teamName team ++ " does not exist"
    Just teamE -> replace (entityKey teamE) team

updateSite :: MonadIO m => Site -> SqlPersistT m ()
updateSite site = do
  siteE' <- getBy $ UniqueSiteToken $ siteToken site
  case siteE' of
    Nothing ->
      throw $ InvalidState $ "Site " ++ siteName site ++ " does not exist"
    Just siteE -> replace (entityKey siteE) site

getOwnerFromStore :: MonadIO m => Site -> SqlPersistT m (Maybe Team)
getOwnerFromStore site =
  case siteOwner site of
    Nothing -> return Nothing
    Just token -> do
      teamE' <- getBy $ UniqueTeamToken token
      return $ entityVal `fmap` teamE'
