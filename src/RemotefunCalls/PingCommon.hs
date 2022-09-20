{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
module RemotefunCalls.PingCommon where

import GHC.Generics ( Generic )
import Data.Serialize ( Serialize )   -- <<< this is from cereal package

import RemotefunCalls.RpcCommon

-- | sate for pne client client-server session
instance RemoteState Integer where
  initState = 0

-- | main application Monad to work with  
type RemotePing a = RSIO Integer a  

-- | data type used as the result of ping call
data PingAnswer = PingAnswer String Integer
  deriving stock (Show, Generic)
  deriving anyclass (Serialize)