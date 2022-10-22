{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Elevators.SafeElev 
            ( module X

            )
            where

import Control.Monad.Trans
import Data.Type.Equality
import Numeric.Natural
import Data.Type.Nat
import Data.Proxy

import Elevators.Floors as X
import Elevators.Operations as X
import Elevators.Moves as X

moveTo :: MonadIO m 
       => FloorK mx to -> ElevatorK mx from 'ClosedK -> m (ElevatorK mx to 'ClosedK)
moveTo flr el = -- undefined
  case decideMove flr (currentFloor el) of 
    StandStill -> pure el
    GoingUp    -> up el >>= moveTo flr
    GoingDown  -> down el >>= moveTo flr 
    
call :: MonadIO m 
     => FloorK mx to -> ElevatorK mx from door -> m (ElevatorK mx to 'OpenedK)
call flr el =  do
  liftIO $ putStrLn $ "Call to: " <> show flr
  case sameFloorK flr (currentFloor el) of
    Just Refl -> ensureOpenedAt flr el
    Nothing   -> ensureClosed el >>= moveTo flr >>= open flr

data SomeFloorK (mx::Nat) where
  MkSomeFloorK :: FloorK mx cur -> SomeFloorK mx

data SomeElevatorK (mx::Nat) where
 MkSomeElevatorK :: ElevatorK mx from door -> SomeElevatorK mx

-- >>>:t (MkFloorK @Nat5 @Nat3)
-- (MkFloorK @Nat5 @Nat3) :: FloorK Nat5 Nat3