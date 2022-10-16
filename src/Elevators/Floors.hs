{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE InstanceSigs #-}
module Elevators.Floors where

import Data.Type.Nat
import Data.Type.Nat.LE
import Data.Type.Equality
import Data.Type.Dec

-- | describes a floor with a maximum  and current number
-- and a relation between cur is less than or equa to max floor num
type GoodFloor mx cur = (SNatI mx, SNatI cur, LE cur mx)

{-
 "SNatI mx means that we can always get a singleton that corresponds to the mx
 natural number.
 SNatI cur does the same for a current floor.
 LE cur mx means that cur is less than or equal to mx.""
-}

-- | every floor should be a GoodFloor as a constraint
-- so when we  a FloorK (or construct MkFloorK) we can have 
-- SNatI of mx and cur and LE relation
data FloorK (mx::Nat) (cur::Nat) where
  MkFloorK :: GoodFloor mx cur => FloorK mx cur

-- >>>MkFloorK  @Nat5 @Nat2
-- FloorK: 2 of 5

-- >>>MkFloorK :: FloorK Nat5 Nat2
-- FloorK: 2 of 5
instance Show (FloorK mx cur)  where
  show :: FloorK mx cur -> String
  show MkFloorK = "FloorK: " <> show (snatToNat (snat :: SNat cur))
                   <> " of " <> show (snatToNat (snat :: SNat mx))                   
                  -- ^^ my version of the above 
                  --  "FloorK: " <> show (snatToNat (snat @cur))
                  --  <> " of " <> show (snatToNat (snat @mx)) 
-- | the constraint if we are allowed to go next level
-- the next level after cur is (S cur) have to LE than/to mx
type BelowTop mx cur = LE (S cur) mx

next :: forall (mx::Nat) (cur::Nat). BelowTop mx cur => FloorK mx cur -> FloorK mx (S cur)
next MkFloorK = MkFloorK -- @mx @(S cur)

-- >>>:t show (MkFloorK  @Nat5 @Nat2)
-- show (MkFloorK  @Nat5 @Nat2) :: String

-- >>>:i Nat5
-- type Nat5 :: Nat
-- type Nat5 = 'S Nat4 :: Nat
--   	-- Defined in ‘Data.Type.Nat’

-- >>>:t S
-- S :: Nat -> Nat

-- >>>:i S
-- type Nat :: *
-- data Nat = ... | S Nat
--   	-- Defined in ‘Data.Nat’

-- >>>:t snatToNat
-- snatToNat :: SNat n -> Nat

-- >>>:t snat
-- snat :: SNatI n => SNat n

-- >>>:i snatToNat
-- snatToNat :: forall n. SNat n -> Nat 	-- Defined in ‘Data.Type.Nat’
