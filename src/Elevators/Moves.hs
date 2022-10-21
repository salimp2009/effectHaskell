{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module Elevators.Moves where

import Data.Type.Nat
import Data.Type.Nat.LE
import Data.Type.Dec
import Data.Type.Equality
import Data.Void

import Elevators.Floors 

data Move mx to from where
  StandStill :: forall mx to. Move mx to to
  GoingUp    :: forall mx to from.BelowTop mx from => Move mx to from
  GoingDown  :: from ~ S fl => Move mx to from 


decideMove :: forall mx to from
            . FloorK mx to -> FloorK mx from -> Move mx to from  
decideMove MkFloorK MkFloorK = --- undefined
   case discreteNat :: Dec (to :~: from) of
    Yes Refl        -> StandStill @mx @to
    No to_neq_from  -> 
      case decideLE :: Dec (LEProof to from) of
        Yes to_le_from ->
            withAboveGround to_le_from to_neq_from GoingDown 
        No  to_gt_from ->
            withLEProof (belowTop to_gt_from) GoingUp
  where
    --belowTop :: LE to mx => Neg (LEProof to from) -> LEProof (S from) mx
    --belowTop neg = leTrans (leSwap neg) leProof
    belowTop :: LE  to mx  => Neg (LEProof to from) -> LEProof (S from) mx
    belowTop neg = leTrans (leSwap neg) leProof

    withAboveGround :: LEProof to from
                    -> Neg (to :~: from) 
                    -> (forall fl. from ~ S fl => r) -> r
    withAboveGround (LESucc _) _ r = r
    withAboveGround LEZero neq r = 
      case snat :: SNat from of
        SZ -> absurd $ neq Refl
        SS -> r
-- >>>:i discreteNat
-- discreteNat :: (SNatI n, SNatI m) => Dec (n :~: m)
--   	-- Defined in ‘Data.Type.Nat’

-- >>>:i leProof
-- type LE :: Nat -> Nat -> Constraint
-- class LE n m where
--   leProof :: LEProof n m
--   	-- Defined in ‘Data.Type.Nat.LE’

-- >>>:i leTrans
-- leTrans :: LEProof n m -> LEProof m p -> LEProof n p
--   	-- Defined in ‘Data.Type.Nat.LE’

-- >>>:i leSwap
-- leSwap ::
--   (SNatI n, SNatI m) => Neg (LEProof n m) -> LEProof ('S m) n
--   	-- Defined in ‘Data.Type.Nat.LE’

-- >>>:i SNatI
-- type SNatI :: Nat -> Constraint
-- class SNatI n where
--   induction :: f 'Z
--                -> (forall (m :: Nat). SNatI m => f m -> f ('S m)) -> f n
--   {-# MINIMAL induction #-}
--   	-- Defined in ‘Data.Type.Nat’
-- instance SNatI 'Z -- Defined in ‘Data.Type.Nat’
-- instance SNatI n => SNatI ('S n) -- Defined in ‘Data.Type.Nat’

-- >>>:i Dec
-- type Dec :: * -> *
-- data Dec a = Yes a | No (Neg a)
--   	-- Defined in ‘Data.Type.Dec’

-- >>>:i Neg
-- type Neg :: * -> *
-- type Neg a = a -> Void
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
