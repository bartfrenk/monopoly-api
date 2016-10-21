{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module API where

import Servant

import Entities
import Types

type LocationAPI = Get '[JSON] [Location] :<|>
                   ReqBody '[JSON] [Location] :> Post '[JSON] [LocationId] :<|>
                   Capture "locationId" LocationId
                     :> "visit" :> Capture "teamId" TeamId
                     :> Post '[JSON] VisitResult

type TeamAPI = ReqBody '[JSON] TeamDetails :> Post '[JSON] TeamId


type MonopolyAPI =
  "teams" :> TeamAPI :<|>
  "locations" :> LocationAPI

api :: Proxy MonopolyAPI
api = Proxy
