{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module Types where

import GHC.Generics
import Data.Aeson
import Data.Text
import Database.Persist.TH

type Latitude = Double

type Longitude = Double

data LocationType
  = Street
  | Station
  | Water
  | Electra
  | Start
  | Jail deriving (Eq, Show, Read, Generic)

type Color = String

type Money = Int
type Rent = Money

instance FromJSON LocationType
instance ToJSON LocationType
derivePersistField "LocationType"

data ChanceCard =
  Question String String
  deriving (Show, Generic)

instance ToJSON ChanceCard

type DiceResult = Int

data VisitResult
  = CanBuy [ChanceCard]
  | OwnedByVisitor
  | OwnedByOther Rent (Maybe DiceResult)
  | LastVisitTooRecent
  | ShouldBeInJail
  | OnWayToStart deriving (Show, Generic)

instance ToJSON VisitResult

data BuyResult

data TeamDetails = TeamDetails {
  name :: !Text
} deriving Generic

instance FromJSON TeamDetails

