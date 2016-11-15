{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module Types where

import Data.Aeson
import Data.Time.Clock
import Data.Word
import Database.Persist.TH
import Data.Time.Format
import GHC.Generics

type Money = Int

type Color = String

type Token = Word16

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
  toJSON s@ToJail = toJSON $ show s
  toJSON s@(ToStart _) = toJSON $ show s
  toJSON s@Free = toJSON $ show s
  toJSON (InJail t) = let fmt = iso8601DateFormat (Just "%H:%M:%S%QZ")
                      in toJSON $ "InJail " ++ formatTime defaultTimeLocale fmt t

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


data QuestionD = QuestionD
  { phrase :: String
  , options :: [String]
  , token :: QuestionToken
  } deriving (Eq, Show, Read, Generic)

instance ToJSON QuestionD

data ChanceCard
  = GoToJail
  | GoToStart Money
  | NoQuestion Token
  | Q QuestionD
  deriving (Show, Eq, Read, Generic)

instance ToJSON ChanceCard

data VisitRes
  = PickCard [ChanceCard]
  | InsufficientMoneyToBuy
  | InsufficientMoneyToRent
  | ReceivedStartBonus Money
  | IllegalVisitWhileInJail
  | PayedRent Money
              String
              [DieResult]
  | TeamPutInJail UTCTime
  | SiteOwnedByVisitor
  | RepeatedVisit VisitRes
  | NoVisitResult
  deriving (Eq, Show, Read, Generic)

instance ToJSON VisitRes

derivePersistField "VisitRes"

derivePersistField "ChanceCard"

derivePersistField "Location"

derivePersistField "TeamStatus"

derivePersistField "UtilityType"

derivePersistField "SiteType"

derivePersistField "TransactionReason"

