{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module Types where

import GHC.Generics
import Data.Aeson
import Database.Persist.TH

type Latitude = Double

type Longitude = Double

data LocationType
  = Street
  | Station
  | Electricity
  | Start
  | Jail deriving (Eq, Show, Read, Generic)

instance FromJSON LocationType
instance ToJSON LocationType
derivePersistField "LocationType"
