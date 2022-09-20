{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module RemotefunCalls.RemoteIO where


import RemotefunCalls.RpcCommon
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Catch
import Network.Connection
import Network.Socket (PortNumber)
import System.IO.Error (isEOFError)

import Data.Serialize hiding (get,put)
import qualified Data.ByteString as BS

{- 
  "Stage0, we decode the first field of an envelope (the size of the payload).
  Stage1, we decode the second field of an envelope to a ByteString.
  Stage2, we decode the ByteString to a value we expect (either a tuple with all
  the parameters or a result of a function call, depending on the side we are on)
  The same stages are used to encode parameters in the opposite order, 
  sice we don't have to report them because no problems expected during encoding"
-}

unEitherStaged :: DecodeStages -> Either String a -> RSIO st a
unEitherStaged stage eValue = either (throwRemote . errMsg ) pure eValue
  where
    errMsg msg = "Decoding error (" <> show stage <> "): " <> msg

runRemote :: RemoteState st => String -> PortNumber -> RSIO st a -> IO a
runRemote host port computation = do
    conn <- remoteConnectTo host port
    res <- runRemoteConn conn computation
    liftIO $ connectionClose conn
    pure res   
    
runRemoteConn :: RemoteState st => Connection -> RSIO st a -> IO a
runRemoteConn conn computation =
    runReaderT (evalStateT (runRem computation) initState) conn    

{- 
  from cereal package and module Serialize 
  encode :: Serialize a => a -> ByteString
  decode :: Serialize a => ByteString -> Either String a
-}

-- The sendRSIO and receiveRSIO functions are responsible 
-- for encoding and decoding at stages 0 and 1
sendRSIO :: Serialize a => a -> RSIO st ()
sendRSIO msg = do 
     conn <- ask 
     liftIO $ connectionPut conn $ buildMsgEnvelope $ encode msg
  where
    buildMsgEnvelope payload = runPut $ do
      putWord64be (fromIntegral $ BS.length payload)
      putByteString payload 

{- 
  newtype RSIO st a = RSIO 
            { runRem :: StateT st (ReaderT Connection IO) a 
            }
-}
receiveRSIO :: Serialize a => RSIO st a
receiveRSIO = ask >>= \conn ->
          recvExact conn msgSizeField
          >>= unEitherStaged Stage0 . runGet getWord64be
          >>= recvExact conn . fromIntegral
          >>= unEitherStaged Stage1 . decode
  where
    recvExact conn sz =
      catch (liftIO $ connectionGetExact conn sz)
            (\e -> if isEOFError e then throwM ConnectionClosed
                   else throwRemote (displayException e))

-- >>>:t RemoteException
-- RemoteException :: String -> RemoteException

-- >>>:i RemoteException
-- type RemoteException :: *
-- data RemoteException = ConnectionClosed | RemoteException String
--   	-- Defined at C:\developer\haskell\effectiveHaskellbook\chapter9Monads\monadTypeClass\src\RemotefunCalls\RpcCommon.hs:17:1
-- instance Show RemoteException
--   -- Defined at C:\developer\haskell\effectiveHaskellbook\chapter9Monads\monadTypeClass\src\RemotefunCalls\RpcCommon.hs:21:10
-- instance Exception RemoteException
--   -- Defined at C:\developer\haskell\effectiveHaskellbook\chapter9Monads\monadTypeClass\src\RemotefunCalls\RpcCommon.hs:34:10
throwRemote :: String -> RSIO st b
throwRemote err_msg = throwM $ RemoteException err_msg

-- >>>:t ConnectionParams
-- ConnectionParams
--   :: HostName
--      -> PortNumber
--      -> Maybe TLSSettings
--      -> Maybe ProxySettings
--      -> ConnectionParams

-- >>>:i ConnectionContext
-- type ConnectionContext :: *
-- data ConnectionContext
--   = ConnectionContext {globalCertificateStore :: !CertificateStore}
--   	-- Defined in ‘Network.Connection.Types’

-- >>>:t initConnectionContext
-- initConnectionContext :: IO ConnectionContext
remoteConnectTo :: String -> PortNumber -> IO Connection
remoteConnectTo host port = do
    connCtx <- initConnectionContext
    connectTo connCtx connParams
  where
    connParams = ConnectionParams host port Nothing Nothing





