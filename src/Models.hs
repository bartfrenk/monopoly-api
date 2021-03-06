{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
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

module Models
  ( module Models
  , module Types
  ) where

import Data.Aeson
import Data.Time.Clock (UTCTime)
import Database.Persist
import Database.Persist.TH
import GHC.Generics
import Types


share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
Team json
  name String
  UniqueTeamName name
  token TeamToken
  UniqueTeamToken token
  money Money
  status TeamStatus
  statusUpdated UTCTime
  deriving Show
  deriving Eq

Site json
  name String
  UniqueSiteName name
  token SiteToken
  UniqueSiteToken token
  location Location
  siteType SiteType
  color Color
  ownerId TeamId Maybe
  price Money Maybe
  updated UTCTime
  deriving Show
  deriving Eq

Visit
  when UTCTime
  siteId SiteId
  teamId TeamId
  result VisitRes Maybe
  deriving Show
  deriving Eq

Question json
  phrase String
  UniquePhrase phrase
  token QuestionToken
  UniqueQuestionToken token
  options [String]
  siteId SiteId Maybe
  answerIndex AnswerIndex
  deriving Show
  deriving Eq

TeamLocation
  when UTCTime
  teamId TeamId
  location Location

Transaction
  when UTCTime
  amount Money
  source TeamId Maybe
  destination TeamId Maybe
  site SiteId Maybe
  reason TransactionReason
|]

type TeamE = Entity Team

type SiteE = Entity Site

type QuestionE = Entity Question

data TeamU = TeamU
  { name :: Maybe String,
    token :: Maybe TeamToken
  } deriving (Eq, Show, Generic)

instance FromJSON TeamU

data SiteU = SiteU
  { name :: String
  , location :: Location
  , siteType :: SiteType
  , color :: Color
  , price :: Maybe Money
  } deriving (Eq, Show, Generic)

instance FromJSON SiteU

data SiteD = SiteD
  { name :: String
  , location :: Location
  , siteType :: SiteType
  , color :: Color
  , token :: SiteToken
  , price :: Maybe Money
  } deriving (Eq, Show, Generic)

instance ToJSON SiteD

toSiteD :: Site -> SiteD
toSiteD Site {..} =
  SiteD
  { name = siteName
  , location = siteLocation
  , siteType = siteSiteType
  , color = siteColor
  , price = sitePrice
  , token = siteToken
  }

data QuestionU = QuestionU
  { phrase :: String
  , options :: [String]
  , site :: Maybe String
  , answerIndex :: AnswerIndex
  } deriving (Eq, Show, Generic)

instance FromJSON QuestionU

toQuestionD :: Question -> QuestionD
toQuestionD Question {..} =
  QuestionD
  { phrase = questionPhrase
  , options = questionOptions
  , token = questionToken
  }
