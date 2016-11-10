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

type DieResult = Int

data TransactionReason
  = Rent
  | Sale
  | StartBonus
  deriving (Eq, Show, Read, Generic)

data Location = Location
  { latitude :: Double
  , longitude :: Double
  } deriving (Eq, Show, Read, Generic)

instance FromJSON Location

instance ToJSON Location

data TeamStatus
  = ToJail
  | ToStart Money
  | InJail UTCTime
  | Free
  deriving (Eq, Show, Read, Generic)

instance FromJSON TeamStatus where
  parseJSON v = read <$> parseJSON v

instance ToJSON TeamStatus where
  toJSON = toJSON . show

data UtilityType
  = Water
  | Electra
  deriving (Eq, Show, Read, Generic)

instance FromJSON UtilityType

instance ToJSON UtilityType

data SyncData = SyncData
  { money :: Money
  , status :: TeamStatus
  } deriving (Eq, Show, Read, Generic)

instance ToJSON SyncData

data SiteType
  = Street
  | Station
  | Utility UtilityType
  | Start
  | Jail
  deriving (Eq, Show, Read, Generic)

instance FromJSON SiteType where
  parseJSON v = read <$> parseJSON v

instance ToJSON SiteType where
  toJSON = toJSON . show

derivePersistField "Location"

derivePersistField "TeamStatus"

derivePersistField "UtilityType"

derivePersistField "SiteType"

derivePersistField "TransactionReason"
