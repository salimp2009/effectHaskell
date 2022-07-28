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