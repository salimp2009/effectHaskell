{-# LANGUAGE QuasiQuotes #-}
--{-# LANGUAGE AllowAmbiguousTypes #-}

module RemotefunCalls.Client where

import Control.Monad
import Control.Monad.Trans

import RemotefunCalls.ClientUtils
import RemotefunCalls.PingCommon

-- [remote| 
--  ping :: RemotePing PingAnswer
--  echo :: String -> RemotePing String
-- |]