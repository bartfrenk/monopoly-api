{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module Types where

import Data.Aeson
import Data.ByteString
import Data.Time.Clock
import Database.Persist.TH
import GHC.Generics

type Currency = Int
type Color = String
type Token = ByteString
type DieResult = Int

data Location = Location {
  latitude :: Double,
  longitude :: Double
  } deriving (Eq, Show, Read, Generic)

instance FromJSON Location
instance ToJSON Location
derivePersistField "Location"

data TeamStatus
  = ToJail
  | ToStart Currency
  | InJail UTCTime
  | Free deriving (Eq, Show, Read, Generic)

instance FromJSON TeamStatus
instance ToJSON TeamStatus
derivePersistField "TeamStatus"

data UtilityType
  = Water
  | Electra deriving (Eq, Show, Read, Generic)
instance FromJSON UtilityType
instance ToJSON UtilityType
derivePersistField "UtilityType"

data SiteType
  = Street
  | Station
  | Utility UtilityType
  | Start
  | Jail deriving (Eq, Show, Read, Generic)

instance FromJSON SiteType
instance ToJSON SiteType
derivePersistField "SiteType"

data ChanceCard = Question [String] Int deriving (Show, Generic, Eq)
instance ToJSON ChanceCard

isCorrectAnswer :: ChanceCard -> Maybe Int -> Bool
isCorrectAnswer (Question _ correct) = maybe True (== correct)

