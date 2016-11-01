{-# LANGUAGE DeriveFunctor #-}
module Client where

import Control.Monad.Except (ExceptT)
import Control.Monad.Free (Free, liftF)
import Data.Time.Clock

import Models hiding (Visit)
type Time = UTCTime

data ClientError token
  = TeamNotFound token
  | SiteNotFound token
  | ChanceCardNotFound token

data VisitResult
  = ChanceCards [ChanceCard]
  | InsufficientFundsToBuy
  | InsufficientFundsToRent
  | StartBonus
  | RentPayed Currency Team [DieResult]
  | TeamJailed
  | VisitedWhileInJail
  | RepeatedVisit
  | NoVisitResult

data BuyResult
  = Success
  | InsufficientFunds
  | WrongAnswer
  | BuyIllegal
  | BuyerNotFound
  | BuySiteNotFound

data ClientF token next
  = NewSite Site (token -> next)
  | NewTeam String (token -> next)
  | NewChanceCard ChanceCard (token -> next)
  | Visit token token Time (Either (ClientError token) VisitResult -> next)
  | Buy token token token (Maybe Int) (Either (ClientError token) BuyResult -> next)
  | Update token Location (Currency -> next) -- idem
  deriving Functor

type ClientAPI token = Free (ClientF token)

newSite :: Site -> ClientAPI token token
newSite site = liftF $ NewSite site id

newTeam :: String -> ClientAPI token token
newTeam name = liftF $ NewTeam name id

newChanceCard :: ChanceCard -> ClientAPI token token
newChanceCard card = liftF $ NewChanceCard card id

visit :: token -> token -> Time
      -> ClientAPI token (Either (ClientError token) VisitResult)
visit siteTk teamTk t = liftF $ Visit siteTk teamTk t id

buy :: token -> token -> token -> Maybe Int
    -> ClientAPI token (Either (ClientError token) BuyResult)
buy siteTk teamTk cardTk manswer = liftF $ Buy siteTk teamTk cardTk manswer id

