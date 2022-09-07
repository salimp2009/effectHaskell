{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
--{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

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

-- >>>withSomeSBool (SomeSBool STrue) fromSBool  
-- True

-- >>>withSomeSBool (toSBool False) fromSBool
-- False
withSomeSBool :: SomeSBool -> (forall (b::Bool). SBool b -> r) -> r
withSomeSBool (SomeSBool sb)  f = f sb

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

program :: MonadLogging b => LoggingMonad b ()
program = do
    logMsg "hello world"  
    pure ()

-- | this version does not work; it gives error;
-- "No instance for (MonadLogging b)
-- arising from a use of ‘runLogging’"
-- the reason for as explained in the book;
{- 
  "The problem is that typeclasses are implemented in
  GHC as implicitly passed variables. By the last line , GHC doesn’t
  know the type of b, and thus can’t find the appropriate
  MonadLogging dictionary—even though MonadLogging is
  total and theoretically GHC should be able to determine
  this"
-}
-- useprogram :: IO ()
-- useprogram = do
--   bool  <- read <$> getLine    
--   withSomeSBool (toSBool bool) $ 
--     \(_ ::SBool b) -> 
--       runLogging @b program

-- | in order to help GHC deduce right instance of
-- Monadlogging b, we can help writing a function
-- that can deliver dictorionaries for any constraint
-- that is total over Bool

-- | dict is a function that requires both constraints to be satisfied 
-- then calls right instance with a given type from Singleton SBool
dict :: (c 'True, c 'False) => SBool b -> Dict (c b)
dict STrue  = Dict
dict SFalse = Dict

-- | rewriting useprogram using dict function
-- use case;
-- -> useprogram True
-- -> hello world
useprogram :: Bool -> IO ()
useprogram bool = do
  withSomeSBool (toSBool bool) $ \(sb :: SBool b) ->
    case dict @MonadLogging sb of
      Dict -> runLogging @b program

