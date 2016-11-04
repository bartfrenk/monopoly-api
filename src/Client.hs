{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
module Client where

import Control.Monad.Free (Free, liftF)
import Data.Time.Clock
import Data.Aeson
import GHC.Generics

import Models hiding (Visit)
type Time = UTCTime

data ClientError token
  = TeamNotFound token
  | SiteNotFound token
  | ChanceCardNotFound token deriving (Show, Eq)


data VisitResult
  = ChanceCards [ChanceResults]
  | InsufficientFundsToBuy
  | InsufficientFundsToRent
  | StartBonus
  | RentPayed Currency Team [DieResult]
  | TeamJailed
  | VisitedWhileInJail
  | RepeatedVisit
  | NoVisitResult deriving (Show, Eq, Generic)

instance ToJSON BuyResult

data BuyResult
  = Success
  | InsufficientFunds
  | WrongAnswer
  | BuyIllegal
  | BuyerNotFound
  | BuySiteNotFound deriving (Show, Eq, Generic)

instance ToJSON VisitResult

data ClientF token next
  = NewSite SiteDetails (token -> next)
  | NewTeam TeamDetails (token -> next)
  | NewChanceCard CardDetails (token -> next)
  | ListSites ([Site] -> next)
  | Visit token token Time (Either (ClientError token) VisitResult -> next)
  | Buy token token token (Maybe Int) (Either (ClientError token) BuyResult -> next)
  | Update token Location (Currency -> next) -- idem
  deriving Functor

type ClientAPI token = Free (ClientF token)

newSite :: SiteDetails -> ClientAPI token token
newSite site = liftF $ NewSite site id

newTeam :: TeamDetails -> ClientAPI token token
newTeam team = liftF $ NewTeam team id

newChanceCard :: CardDetails -> ClientAPI token token
newChanceCard card = liftF $ NewChanceCard card id

listSites :: ClientAPI token [Site]
listSites = liftF $ ListSites id

visit :: token -> token -> Time
      -> ClientAPI token (Either (ClientError token) VisitResult)
visit siteTk teamTk t = liftF $ Visit siteTk teamTk t id

buy :: token -> token -> token -> Maybe Int
    -> ClientAPI token (Either (ClientError token) BuyResult)
buy siteTk teamTk cardTk manswer = liftF $ Buy siteTk teamTk cardTk manswer id

