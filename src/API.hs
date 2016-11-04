{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module API (module API, ClientError) where

import Servant

import Models
import Client

type SiteAPI = Get '[JSON] [Site] :<|>
                   ReqBody '[JSON] [SiteDetails] :> Post '[JSON] [Token] :<|>
                   Capture "location" Token
                     :> "visit" :> Capture "team" Token
                     :> Post '[JSON] VisitResult :<|>
                   Capture "location" Token
                     :> "buy" :> Capture "team" Token
                     :> Capture "card" Token
                     :> QueryParam "answer" Int
                     :> Post '[JSON] BuyResult

type TeamAPI = ReqBody '[JSON] TeamDetails :> Post '[JSON] Token

type CardAPI = ReqBody '[JSON] [CardDetails] :> Post '[JSON] [Token]

type MonopolyAPI =
  "locations" :> SiteAPI :<|>
  "teams" :> TeamAPI :<|>
  "cards" :> CardAPI

api :: Proxy MonopolyAPI
api = Proxy
