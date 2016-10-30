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

toGame :: ClientAPI token a -> GameAPI token a
toGame = foldFree toGame'

toGame' :: ClientF token a -> GameAPI token a

toGame' (Visit site visitor t cont) = do
  repeated <- isRepeatedVisit site visitor
  if repeated then return $ cont RepeatedVisit
  else case (teamStatus visitor, siteType site, sitePrice site) of

    (ToJail, Jail, _) ->
      do putInJail visitor
         return $ cont TeamJailed

    (ToStart _, Start, _) ->
      do payStartBonus visitor
         return $ cont StartBonus

    (InJail s, _, _) ->
      -- REVIEW: this might better go at the top
      if diffUTCTime t s > jailTime then do
        visitor' <- freeFromJail visitor
        toGame' (Visit site visitor' t cont)
      else return $ cont VisitedWhileInJail

    (Free, _, Just price) -> do
      mowner <- getOwner site
      case mowner of

        Just owner -> do
          (rent, dice) <- getRent site
          success <- transfer rent (TeamAcc visitor) (TeamAcc owner)
          if success
            then return $ cont (RentPayed rent owner dice)
            else return $ cont InsufficientFundsToRent

        Nothing ->
          if price <= teamWallet visitor then do
            chanceCards <- drawChanceCards 3
            return $ cont (ChanceCards chanceCards)
          else return $ cont InsufficientFundsToBuy

    _ ->
      return $ cont NoVisitResult

toGame' (Buy site team card manswer cont) =
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
          return $ cont Success
        else return $ cont InsufficientFunds
      else return $ cont WrongAnswer

    _ -> return $ cont BuyIllegal

toGame' (NewTeam name cont) = do
  token <- createTeam name startingMoney
  return $ cont token

toGame' (NewSite site cont) = do
  token <- createSite site
  return $ cont token

toGame' (NewChanceCard card cont) = do
  token <- createChanceCard card
  return $ cont token

