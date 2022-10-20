{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
module Elevators.Moves where

import Data.Type.Nat
import Data.Type.Nat.LE
import Data.Type.Dec
import Data.Type.Equality
import Data.Void

import Elevators.Floors 

data Move mx to from where
  StandStill :: Move mx to to
  GoingUp    :: BelowTop mx from => Move mx to from
  GoingDown  :: from ~ S fl => Move mx to from 


decideMove :: forall mx to from
            . FloorK mx to -> FloorK mx from -> Move mx to from  
decideMove = undefined


-- >>>:i discreteNat
-- discreteNat :: (SNatI n, SNatI m) => Dec (n :~: m)
--   	-- Defined in ‘Data.Type.Nat’

-- >>>:i Dec
-- type Dec :: * -> *
-- data Dec a = Yes a | No (Neg a)
--   	-- Defined in ‘Data.Type.Dec’

-- >>>:i decideLE
-- decideLE :: (SNatI n, SNatI m) => Dec (LEProof n m)
--   	-- Defined in ‘Data.Type.Nat.LE’

-- >>>:i LEProof
-- type role LEProof nominal nominal
-- type LEProof :: Nat -> Nat -> *
-- data LEProof n m where
--   LEZero :: LEProof 'Z m
--   LESucc :: LEProof n1 m1 -> LEProof ('S n1) ('S m1)
--   	-- Defined in ‘Data.Type.Nat.LE’
-- instance [safe] (SNatI n, SNatI m) => Decidable (LEProof n m)
--   -- Defined in ‘Data.Type.Nat.LE’
