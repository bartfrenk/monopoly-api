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

module Models (module Models, module Types) where

import Database.Persist.TH
import Data.Time.Clock (UTCTime)
import Data.ByteString.Char8 (pack)

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
  UniqueLocationName name
  deriving Show

Visit
  when UTCTime
  locationToken Token
  teamToken Token

|]

instance Eq Team where
  team1 == team2 = teamToken team1 == teamToken team2

createTeam :: String -> Team
createTeam name' =
  Team name' (pack name') 1000 Free

instance Eq Site where
  site1 == site2 = siteToken site1 == siteToken site2

createStreet :: String -> Location -> Color -> Currency -> Site
createStreet name' location color price =
  Site name' (pack name') location Street color Nothing (Just price)

data TeamDetails = TeamDetails {
  name :: String
}

data SiteDetails = SiteDetails {
  name :: String
}
