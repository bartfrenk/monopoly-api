{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module API (module API, ActionErr) where

import Servant

import Models
import Actions
import Game

type SiteAPI = Get '[JSON] [SiteE]
          :<|> ReqBody '[JSON] [SiteU] :> Post '[JSON] [SiteE]
          :<|> Capture "siteT" SiteToken :>
                  "visit" :> Capture "teamT" TeamToken
                          :> Post '[JSON] VisitRes
          :<|> Capture "siteT" SiteToken :>
          "buy" :> Capture "teamT" TeamToken
                          :> ReqBody '[JSON] BuyPermission
                          :> Post '[JSON] BuyRes

type TeamAPI = ReqBody '[JSON] TeamU :> Post '[JSON] TeamE
          :<|> Capture "teamT" TeamToken :> (
                 "sync" :> ReqBody '[JSON] Location
                        :> Post '[JSON] (Money, TeamStatus))

type QuestionAPI = ReqBody '[JSON] [QuestionU] :> Post '[JSON] [QuestionE]

type MonopolyAPI = "locations" :> SiteAPI
              :<|> "teams" :> TeamAPI
              :<|> "questions" :> QuestionAPI

api :: Proxy MonopolyAPI
api = Proxy
