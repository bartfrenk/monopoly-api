{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE EmptyDataDecls #-} {-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}

module Models (module Models, module Types) where

import Database.Persist.TH
import Data.Time.Clock (UTCTime)
import Data.ByteString.Char8 (pack)
import Data.Aeson
import GHC.Generics

import Orphans()

import Types

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Team json
  name String
  UniqueTeamName name
  token Token
  UniqueTeamToken token
  wallet Currency
  status TeamStatus
  deriving Show

Site json
  name String
  token Token
  location Location
  type SiteType
  color Color
  owner Token Maybe
  price Currency Maybe
  UniqueSiteToken token
  UniqueSiteName name
  deriving Show

Visit
  when UTCTime
  locationToken Token
  teamToken Token

ChanceCard
  token Token
  UniqueCardToken token
  question String
  UniqueQuestion question
  options [String]
  siteToken Token Maybe
  answer Int
  deriving Show
|]

instance Eq Team where
  team1 == team2 = teamToken team1 == teamToken team2

createTeam :: String -> Team
createTeam name' =
  Team name' (pack name') 1000 Free

instance Eq Site where
  site1 == site2 = siteToken site1 == siteToken site2

instance Eq ChanceCard where
  card1 == card2 = chanceCardToken card1 == chanceCardToken card2

createStreet :: String -> Location -> Color -> Currency -> Site
createStreet name' location color price =
  Site name' (pack name') location Street color Nothing (Just price)

data TeamDetails = TeamDetails {
  name :: String
} deriving (Show, Generic)

instance ToJSON TeamDetails
instance FromJSON TeamDetails

data SiteDetails = SiteDetails {
  name :: String,
  location :: Location,
  sitetype :: SiteType,
  color :: Color,
  price :: Maybe Currency
} deriving (Show, Generic)

-- TODO: no automatic deriving here
instance ToJSON SiteDetails
instance FromJSON SiteDetails

data CardDetails = CardDetails {
  question :: String,
  options :: [String],
  cardSiteName :: Maybe String,
  answer :: Int
} deriving (Show, Generic)

data ChanceResults
  = GoToJail
  | GoToStart Currency
  | Question [String] Token
  deriving (Show, Eq, Generic)

instance ToJSON ChanceResults

instance ToJSON CardDetails
instance FromJSON CardDetails

isCorrectAnswer :: ChanceCard -> Maybe Int -> Bool
isCorrectAnswer ChanceCard{..} manswer =
  case manswer of
    Nothing -> True
    Just answer -> chanceCardAnswer == answer
