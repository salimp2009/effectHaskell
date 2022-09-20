{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
-- {-# LANGUAGE InstanceSigs #-}
module RemotefunCalls.RpcCommon where

import Data.ByteString (ByteString)
import Network.Connection ( Connection )
import Control.Monad.Reader ( MonadIO, MonadReader, ReaderT )
import Control.Monad.State ( MonadIO, MonadState, StateT(StateT) )
import Control.Monad.Catch ( MonadThrow, Exception, MonadCatch )

-- | message size in bytes
msgSizeField :: Int
msgSizeField = 8 


data RemoteException = 
               ConnectionClosed
             | RemoteException String

instance Show RemoteException where
  show ConnectionClosed = "Closed"
  show (RemoteException msg) = "Remote Exception: " <> msg

-- >>>:i Exception
-- type Exception :: * -> Constraint
-- class (Typeable e, Show e) => Exception e where
--   toException :: e -> SomeException
--   fromException :: SomeException -> Maybe e
--   displayException :: e -> String
--   	-- Defined in ‘GHC.Exception.Type’

-- | there is no type class method to implement
instance Exception RemoteException

class RemoteState a where
    initState :: a

instance RemoteState () where
  initState = ()

-- >>>:i StateT
-- type role StateT nominal representational nominal
-- type StateT :: * -> (* -> *) -> * -> *
-- newtype StateT s m a = StateT {runStateT :: s -> m (a, s)}
--   	-- Defined in ‘Control.Monad.Trans.State.Lazy’

-- >>>:i ReaderT
-- type role ReaderT representational representational nominal
-- type ReaderT :: * -> (* -> *) -> * -> *
-- newtype ReaderT r m a = ReaderT {runReaderT :: r -> m a}
--   	-- Defined in ‘Control.Monad.Trans.Reader’
newtype RSIO st a = RSIO 
            { runRem :: StateT st (ReaderT Connection IO) a 
            }
            deriving newtype (Functor, Applicative, Monad, MonadIO,
                              MonadReader Connection,
                              MonadState st,
                              MonadThrow, MonadCatch)

type Operation = String
type RemoteAction st a b = a -> RSIO st a
type RPCTable st = [(Operation, RemoteAction st ByteString ByteString)]                              

data DecodeStages = Stage0 | Stage1 | Stage2
    deriving Show