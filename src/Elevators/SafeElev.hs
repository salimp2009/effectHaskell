{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
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