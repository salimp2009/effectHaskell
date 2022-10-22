--{-# LANGUAGE CPP #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}


module Elevators.Operations where

-- | Safe Elev Operations

import Data.Type.Nat
import Data.Singletons.TH
import Control.Monad.Trans
import Prelude.Singletons

import qualified Elevators.LowLevelElev as LL
import Elevators.Floors

singletons [d| 
    data DoorK
      = OpenedK
      | ClosedK
      deriving (Eq, Show)  
    |]

data ElevatorK (mx::Nat) (cur::Nat) (door::DoorK) where
  MkElevatorK :: SingI doorK => FloorK mx cur -> ElevatorK mx cur doorK

currentFloor ::  forall mx cur doorK. ElevatorK mx cur doorK -> FloorK mx cur
currentFloor (MkElevatorK flr) = flr

currentDoor :: forall mx cur doorK. ElevatorK mx cur doorK -> DoorK
currentDoor (MkElevatorK _) = fromSing (sing :: Sing doorK)

instance Show (ElevatorK mx cur doorK) where
  show el = "ElevatorK {currentFloorK = " <> show (currentFloor el) 
            <> ", doorK = " <> show (currentDoor el) <> "}"

up :: (BelowTop mx cur, MonadIO m) 
   => ElevatorK mx cur 'ClosedK -> m (ElevatorK mx (S cur) 'ClosedK)  
up (MkElevatorK flr) = do
    liftIO  LL.up           
    pure (MkElevatorK $ next flr)

down :: MonadIO m 
     => ElevatorK mx (S cur) 'ClosedK -> m (ElevatorK mx cur 'ClosedK)
down (MkElevatorK flr) = do
    liftIO LL.down
    pure (MkElevatorK $ prev flr)

open :: MonadIO m 
     => FloorK mx cur -> ElevatorK mx cur 'ClosedK -> m (ElevatorK mx cur 'OpenedK)
open _ (MkElevatorK flr) = do
    liftIO LL.open
    pure (MkElevatorK flr)

close :: MonadIO m 
       => FloorK mx cur -> ElevatorK mx cur 'OpenedK -> m (ElevatorK mx cur 'ClosedK)
close _ (MkElevatorK flr) = do
   liftIO LL.close
   pure (MkElevatorK flr)    

-- ^^ open close function needs to be refactored to check that we are at same floor   
ensureClosed :: forall mx cur doorK m. MonadIO m 
             => ElevatorK mx cur doorK -> m (ElevatorK mx cur 'ClosedK)
ensureClosed el@(MkElevatorK flr) =
      case sing :: Sing doorK of  
        SClosedK -> pure el
        SOpenedK -> close flr el

-- >>>:i sing      
-- type SingI :: forall {k}. k -> Constraint
-- class SingI a where
--   sing :: Sing a
--   	-- Defined in ‘Data.Singletons’


ensureOpenedAt :: forall mx cur doorK m. MonadIO m
               => FloorK mx cur -> ElevatorK mx cur doorK -> m (ElevatorK mx cur 'OpenedK)
ensureOpenedAt flr el@(MkElevatorK _) =
  case sing :: Sing doorK of 
    SOpenedK -> pure el
    SClosedK -> open flr el
