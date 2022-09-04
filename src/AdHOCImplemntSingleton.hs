{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module AdHOCImplemntSingleton where

import Control.Monad.Trans.Writer
import Data.Constraint (Dict (..))
import Data.Foldable (for_)
import Data.Kind (Type)


data SBool (b::Bool) where
  STrue  :: SBool 'True
  SFalse :: SBool 'False

-- >>>fromSBool STrue
-- True

-- >>>fromSBool SFalse
-- False
fromSBool :: SBool b -> Bool
fromSBool STrue  = True
fromSBool SFalse = False

data SomeSBool where
  SomeSBool :: SBool b -> SomeSBool

-- >>>withSomeBool (SomeSBool STrue) fromSBool  
-- True

-- >>>withSomeBool (toSBool False) fromSBool
-- False
withSomeBool :: SomeSBool -> (forall (b::Bool). SBool b -> r) -> r
withSomeBool (SomeSBool sb)  f = f sb

toSBool :: Bool -> SomeSBool
toSBool True = SomeSBool STrue
toSBool False= SomeSBool SFalse

-- | example of usefull of singletons; ??
-- Build monad stack to conditionally log messages
-- messages will be for debugging purposes only
-- we can conditionally choose our monad stack depending on a runtime value; ??

-- | Beginning with a typeclass indexed over Bool
-- it has an assocaiate type family to select the correct Monad stack
-- plus some methods to work with the stack
class Monad (LoggingMonad b) => MonadLogging (b::Bool) where
  type LoggingMonad b = (r :: Type -> Type) | r -> b         
  logMsg :: String -> LoggingMonad b ()
  runLogging :: LoggingMonad b a -> IO a

  -- ^ | r->b is known as type family dependency
  -- ^ it means if we know LoggingMonad b we can infer b
  
  -- | 'False instance will be used to ignore any attempts to log messages
instance MonadLogging 'False where
  type LoggingMonad 'False = IO
  logMsg _ = pure () 
  runLogging = id 

instance MonadLogging 'True where
  type LoggingMonad 'True = WriterT [String] IO
  logMsg s = tell [s]
  runLogging m = do
    (a, w) <- runWriterT m
    for_ w putStrLn
    pure a

