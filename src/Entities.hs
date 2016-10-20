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

module Entities where

import Database.Persist.TH
import Database.Persist.Sql (Entity(..))
import Data.Aeson
import Data.Text
import Data.Time.Clock
import GHC.Generics

import Types

share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
Location json
  name Text
  UniqueLocationName name
  type LocationType
  price Double Maybe
  latitude Latitude
  longitude Longitude
  deriving Show

Team json
  name Text
  UniqueTeamName name
  money Int
  deriving Show
|]
