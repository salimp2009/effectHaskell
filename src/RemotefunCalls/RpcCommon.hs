{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
module RemotefunCalls.RpcCommon where

import Data.ByteString (ByteString)
import Network.Connection
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Catch