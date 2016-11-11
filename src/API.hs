{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module API (module API, ActionErr) where

import Servant

import Models
import Handlers
import Game

type Options = Verb 'OPTIONS 200

type SiteAPI = Get '[JSON] [SiteD]
          :<|> Options '[JSON] NoContent
          :<|> ReqBody '[JSON] [SiteU] :> Post '[JSON] [SiteE]
          :<|> Options '[JSON] NoContent
          :<|> Capture "siteT" SiteToken :>
                  "visit" :> Capture "teamT" TeamToken
                          :> Post '[JSON] VisitRes
          :<|> Capture "siteT" SiteToken :>
                  "visit" :> Capture "teamT" TeamToken
                          :> Options '[JSON] NoContent
          :<|> Capture "siteT" SiteToken :>
                    "buy" :> Capture "teamT" TeamToken
                          :> ReqBody '[JSON] BuyPermission
                          :> Post '[JSON] BuyRes
          :<|> Capture "siteT" SiteToken :>
                    "buy" :> Capture "teamT" TeamToken
                          :> Post '[JSON] NoContent


type TeamAPI = ReqBody '[JSON] TeamU :> Post '[JSON] TeamE
          :<|> Options '[JSON] NoContent
          :<|> Capture "teamT" TeamToken :> (
                 "sync" :> ReqBody '[JSON] Location
                        :> Post '[JSON] SyncData)
          :<|> Capture "teamT" TeamToken :> (
                 "sync" :> Options '[JSON] NoContent)
          :<|> Capture "teamT" TeamToken :> (
                 "to-jail" :> Post '[JSON] NoContent)
          :<|> Capture "teamT" TeamToken :> (
                 "to-jail" :> Options '[JSON] NoContent)
          :<|> Capture "teamT" TeamToken :> (
                 "to-start" :> QueryParam "amount" Money
                            :> Post '[JSON] NoContent)
          :<|> Capture "teamT" TeamToken :> (
                 "to-start" :> Options '[JSON] NoContent)





type QuestionAPI = ReqBody '[JSON] [QuestionU] :> Post '[JSON] [QuestionE]
              :<|> Options '[JSON] NoContent

type MonopolyAPI = "locations" :> SiteAPI
              :<|> "teams" :> TeamAPI
              :<|> "questions" :> QuestionAPI

api :: Proxy MonopolyAPI
api = Proxy
