{-# LANGUAGE CPP #-}
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
#if __GLASGOW_HASKELL__ >= 810
{-# LANGUAGE StandaloneKindSignatures #-}
#endif

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
      = Opened
      | Closed
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
            
            
            