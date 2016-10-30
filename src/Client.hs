{-# LANGUAGE DeriveFunctor #-}
module Client where

import Control.Monad.Free (Free, liftF)
import Data.Time.Clock

import Models hiding (Visit)
type Time = UTCTime

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

data ClientF token next
  = NewSite Site (token -> next)
  | NewTeam String (token -> next)
  | NewChanceCard ChanceCard (token -> next)
  | Visit Site Team Time (VisitResult -> next) -- tokens instead of entities
  | Buy Site Team ChanceCard (Maybe Int) (BuyResult -> next) -- idem
  | Update Team Location (Currency -> next) -- idem
  deriving Functor

type ClientAPI token = Free (ClientF token)

newSite :: Site -> ClientAPI token token
newSite site = liftF $ NewSite site id

newTeam :: String -> ClientAPI token token
newTeam name = liftF $ NewTeam name id

newChanceCard :: ChanceCard -> ClientAPI token token
newChanceCard card = liftF $ NewChanceCard card id

visit :: Site -> Team -> Time -> ClientAPI token VisitResult
visit site team t = liftF $ Visit site team t id

buy :: Site -> Team -> ChanceCard -> Maybe Int -> ClientAPI token BuyResult
buy site team card manswer = liftF $ Buy site team card manswer id

