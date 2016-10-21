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

module Entities (module Entities, Entity) where

import Database.Persist.TH
import Data.Text
import Data.Time.Clock
import Database.Persist (Entity)

import Types

share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
Location json
  name Text
  UniqueLocationName name
  type LocationType
  price Double Maybe
  color Color
  latitude Latitude
  longitude Longitude
  deriving Show

Team json
  name Text
  UniqueTeamName name
  startingMoney Int
  jailTime Bool
  goBonus Money
  deriving Show

Owns
  teamId TeamId
  locationId LocationId
  since UTCTime
  deriving Show

Visit
  when UTCTime
  locationId LocationId
  teamId TeamId
  deriving Show

Transaction
  src TeamId
  dest TeamId
  amount Money
  visitId VisitId Maybe
|]
