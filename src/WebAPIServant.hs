{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}

module WebAPIServant where

import GHC.Generics
import Data.Aeson
import Servant
import Servant.HTML.Blaze
import qualified Text.Blaze.Html5 as H
import Network.Wai.Handler.Warp

-- | Using Servant to implement web services
-- warp as an HTTP Server
-- "blaze-html and servant-blaze to generate and return HTML documents"
-- aeson for JSON related 

data RatingS = Bad | Good | Great 
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

type BookIDS = Int

data ServiceStatusS = Ok | Down