{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module API where

import Servant

import Entities

type MonopolyAPI =
  "teams" :>
       ReqBody '[JSON] Team :> Post '[JSON] TeamId :<|>
  "locations" :> (
       Get '[JSON] [Location] :<|>
       ReqBody '[JSON] [Location] :> Post '[JSON] [LocationId])

api :: Proxy MonopolyAPI
api = Proxy
