module ToPersist where

import Control.Monad.Trans (MonadIO, liftIO)
import Control.Monad.Free (foldFree)
import Control.Exception
import Crypto.Random
import Data.Typeable
import Data.Time.Clock

import Database.Persist.Sql

import Game
import Models

data InvalidState = InvalidState String deriving (Typeable, Show)

instance Exception InvalidState

toPersist :: MonadIO m => GameAPI Token a -> SqlPersistT m a
toPersist = foldFree toPersist'

toPersist' :: MonadIO m => GameF Token a -> SqlPersistT m a

toPersist' (GetRent site cont) = undefined

toPersist' (Transfer amount src dest cont) =
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


toPersist' (CreateSite site cont) = undefined

toPersist' (CreateChanceCard card cont) = undefined

toPersist' (DrawChanceCards count cont) = undefined

toPersist' (IsRepeatedVisit site team cont) = undefined

-- toPersist' (Transfer amount src dest cont) =
--   case (src, dest) of
--     (Bank, Bank) -> return $ cont True
--     (Bank, TeamAcc dest') -> updateTeam { dest' { teamWallet =

-- TODO: handle duplicate token
toPersist' (CreateTeam name funds cont) = do
  token <- liftIO createToken
  let team = Team name token funds Free
  _ <- insert team
  return $ cont token

toPersist' (PayStartBonus team cont) = do
  let team' =
        case teamStatus team of
          ToStart bonus -> team { teamWallet = teamWallet team + bonus,
                                  teamStatus = Free }
          _ -> team
  updateTeam team'
  return cont

toPersist' (PutInJail team cont) = do
  t <- liftIO getCurrentTime
  updateTeam $ team { teamStatus = InJail t }
  return cont

toPersist' (FreeFromJail team cont) = do
  let team' = team { teamStatus = Free }
  updateTeam team'
  return $ cont team'

toPersist' (MakeOwner site team cont) = do
  updateSite $ site { siteOwner = Just $ teamToken team }
  return cont

toPersist' (GetOwner site cont) = do
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
