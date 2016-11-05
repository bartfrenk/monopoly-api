{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module Types where

import Data.Aeson
import Data.Time.Clock
import Data.Word
import Database.Persist.TH
import GHC.Generics

type Money = Int

type Color = String

type Token = Word64

type SiteToken = Token

type TeamToken = Token

type QuestionToken = Token

type AnswerIndex = Word

data Location = Location {
  latitude :: Double,
  longitude :: Double
  } deriving (Eq, Show, Read, Generic)

instance FromJSON Location

instance ToJSON Location

data TeamStatus
  = ToJail
  | ToStart Money
  | InJail UTCTime
  | Free deriving (Eq, Show, Read, Generic)

instance FromJSON TeamStatus

instance ToJSON TeamStatus

data UtilityType
  = Water
  | Electra deriving (Eq, Show, Read, Generic)

instance FromJSON UtilityType

instance ToJSON UtilityType

data SiteType
  = Street
  | Station
  | Utility UtilityType
  | Start
  | Jail deriving (Eq, Show, Read, Generic)

instance FromJSON SiteType where
  parseJSON v = read <$> parseJSON v

instance ToJSON SiteType where
  toJSON = toJSON . show

derivePersistField "Location"
derivePersistField "TeamStatus"
derivePersistField "UtilityType"
derivePersistField "SiteType"
