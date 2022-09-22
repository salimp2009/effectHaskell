{-# LANGUAGE TemplateHaskell #-}
module RemotefunCalls.Server where

import Control.Monad.State
import RemotefunCalls.ServerUtils
import RemotefunCalls.PingCommon

ping :: RemotePing PingAnswer
ping = do
  modify (+1)
  n <- get
  liftIO $ putStrLn $ "Ping received/answered with " <> show n
  pure $ PingAnswer "OK" n
echo :: String -> RemotePing String
echo msg = do
    liftIO $ putStrLn $ "Echo message: " <> msg
    pure msg

-- genServer ['ping, 'echo]