{-# LANGUAGE RankNTypes, ScopedTypeVariables #-}
{-# LANGUAGE DerivingVia #-}
--{-# LANGUAGE TypeApplications #-}

module ExistentialsForUnsafeIO where

import Data.IORef ( newIORef, readIORef, writeIORef, IORef(..) )
import System.IO.Unsafe (unsafePerformIO)

newtype ST s a = ST 
  {  unsafeRunST :: a}

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

modifySTRef :: STRef s a -> (a -> a) -> ST s ()
modifySTRef ref f = do
    a <- readSTRef ref
    writeSTRef ref (f a)

-- | original implementation; 
--  runST = unsafeRun does not work
-- because runST can only work ST a due to (forall s . ST s a)
-- parameter s is hidden for runST and unsafeRunST expects ST s a
-- GHC calls type s as rigid skolem type variable
-- Rigid variables are those that are constrained by a type signature written
-- by a programmer—in other words, they are not allowed
-- to be type inferred.

-- use case;
-- >>> runST safeExample
-- "hello safeIO"

-- >>> runST (newSTRef "does not work")
-- Couldn't match type ‘a’ with ‘STRef s String’
-- Expected: ST s a
--   Actual: ST s (STRef s String)
--   because type variable ‘s’ would escape its scope
-- This (rigid, skolem) type variable is bound by
--   a type expected by the context:
--     forall s. ST s a
--   at C:\developer\haskell\effectiveHaskellbook\chapter9Monads\monadTypeClass\src\ExistentialsForUnsafeIO.hs:63:8-33
runST ::forall a.  (forall s . ST s a) -> a      
runST (ST a) = a  

-- | incomplete; shown only for the problems above
runST2 ::(forall s . ST s (STRef s Bool )) -> STRef s Bool
runST2 x = undefined

safeExample :: ST s String
safeExample = do
  ref <- newSTRef "hello"
  modifySTRef ref (++ " safeIO")
  readSTRef ref
