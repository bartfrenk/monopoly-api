module ToGame where

import Control.Monad.Free (foldFree)
import Data.Time.Clock

import Models hiding (Visit, createTeam)
import Game
import Client

jailTime :: NominalDiffTime
jailTime = 1000

startingMoney :: Currency
startingMoney = 1000


interpret :: ClientAPI token a -> GameAPI token a
interpret = foldFree interpret'

interpret' :: ClientF token a -> GameAPI token a

interpret' (Visit siteTk visitorTk t cont) = do
  site' <- getSite siteTk
  visitor' <- getTeam visitorTk
  case (site', visitor') of
    (Nothing, _) -> return $ cont $ Left $ SiteNotFound siteTk
    (_, Nothing) -> return $ cont $ Left $ TeamNotFound visitorTk
    (Just site, Just visitor) -> doVisit site visitor t cont

interpret' (Buy siteTk buyerTk cardTk manswer cont) = do
  site' <- getSite siteTk
  buyer' <- getTeam buyerTk
  card' <- getChanceCard cardTk
  case (site', buyer', card') of
    (Nothing, _, _) -> return $ cont $ Left $ SiteNotFound siteTk
    (_, Nothing, _) -> return $ cont $ Left $ TeamNotFound buyerTk
    (_, _, Nothing) -> return $ cont $ Left $ ChanceCardNotFound cardTk
    (Just site, Just buyer, Just card) ->
      doBuy site buyer card manswer cont

interpret' (NewTeam team cont) = do
  token <- createTeam team startingMoney
  return $ cont token

interpret' (NewSite site cont) = do
  token <- createSite site
  return $ cont token

interpret' (NewChanceCard card cont) = do
  token <- createChanceCard card
  return $ cont token

interpret' (ListSites cont) = do
  sites <- getSites
  return $ cont sites

doVisit :: Site -> Team -> Time
        -> (Either (ClientError token) VisitResult -> a) -> GameAPI token a
doVisit site visitor t cont = do
  repeated <- isRepeatedVisit site visitor
  if repeated then return $ cont $ Right RepeatedVisit
  else case (teamStatus visitor, siteType site, sitePrice site) of

    (ToJail, Jail, _) ->
      do putInJail visitor
         return $ cont $ Right TeamJailed

    (ToStart _, Start, _) ->
      do payStartBonus visitor
         return $ cont $ Right StartBonus

    (InJail s, _, _) ->
      if diffUTCTime t s > jailTime then do
        visitor' <- freeFromJail visitor
        doVisit site visitor' t cont
      else return $ cont $ Right VisitedWhileInJail

    (Free, _, Just price) -> do
      mowner <- getOwner site
      case mowner of

        Just owner -> do
          (rent, dice) <- getRent site
          success <- transfer rent (TeamAcc visitor) (TeamAcc owner)
          if success
            then return $ cont $ Right (RentPayed rent owner dice)
            else return $ cont $ Right InsufficientFundsToRent

        Nothing ->
          if price <= teamWallet visitor then do
            chanceCards <- drawChanceCards 3
            return $ cont $ Right (ChanceCards chanceCards)
          else return $ cont $ Right InsufficientFundsToBuy

    _ ->
      return $ cont $ Right NoVisitResult

doBuy :: Site -> Team -> ChanceCard -> Maybe Int
      -> (Either (ClientError token) BuyResult -> a) -> GameAPI token a
doBuy site team card manswer cont =
  let status = teamStatus team
      sitetype = siteType site
      mprice = sitePrice site
      owner = siteOwner site
  in case (status, sitetype, mprice, owner) of

    (Free, _, Just price, Nothing) ->
      if isCorrectAnswer card manswer then do
        success <- transfer price (TeamAcc team) Bank
        if success then do
          makeOwner site team
          return $ cont $ Right Success
        else return $ cont $ Right InsufficientFunds
      else return $ cont $ Right WrongAnswer

    _ -> return $ cont $ Right BuyIllegal
