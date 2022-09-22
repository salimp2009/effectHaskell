--{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module RemotefunCalls.ClientUtils 
            ( module RemotefunCalls.RpcCommon
            , module RemotefunCalls.RemoteIO
            , remote
            , callRemote
            ) 
            where

import RemotefunCalls.RpcCommon
import Data.Serialize
import RemotefunCalls.RemoteIO
import RemotefunCalls.DeclsGenerator (remote) 


callRemote :: forall a b st. (Serialize a, Serialize b) => Operation -> RemoteAction st a b
callRemote operation params = do
        sendRSIO (operation, encode params)
        answer <- receiveRSIO
        unEitherStaged Stage2 (decode answer)

 
    
