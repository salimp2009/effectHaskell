module ExistentialsForUnsafeIO where

import Data.IORef
import System.IO.Unsafe (unsafePerformIO)

newtype ST s a = ST 
  { unsafeRunST :: a}

instance Functor (ST s) where
  fmap f (ST a) = seq a . ST $ f a

instance Applicative (ST s) where
  pure = ST
  ST f <*> ST a = seq f. seq a. ST $ f a 

instance Monad (ST s) where
  ST a >>= f =  seq a $ f a

newtype STRef s a = STRef
  { unSTRef :: IORef a} 
  
-- | creating new STRef;
-- newIORef    :: a -> IO (IORef a)
-- unsafePerformIO :: IO (IORef a) -> IORef a
-- STRef :: IORef a -> STRef s a  
newSTRef :: a -> ST s (STRef s a)
newSTRef = pure . STRef . unsafePerformIO . newIORef

-- | read STREf
-- unSTRef :: STRef s a -> IORef a
-- readIORef :: IORef a -> IO a
-- unsafePerformIO :: IO a -> a
-- pure :: a -> ST s a
readSTRef:: STRef s a -> ST s a
readSTRef = pure . unsafePerformIO . readIORef . unSTRef

-- | write STRef
-- ref :: STRef s a
-- unSTRef :: STRef s a -> IORef a
-- writeIORef :: IORef a -> a -> IO ()
-- unsafePerformIO :: IO () -> () or :: IO a -> a
-- pure :: () -> ST s ()
writeSTRef :: STRef s a -> a -> ST s ()
writeSTRef ref =  pure. unsafePerformIO . writeIORef (unSTRef ref)