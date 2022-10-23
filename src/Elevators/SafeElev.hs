{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE PolyKinds #-}

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

-- | using recursion to introduce all the SNatI n instances 
-- for all n less than S n 
-- Once we have all SNatI n instances, GHC easily constructs
-- the requested floor's SNatI instance ->
mkSomeFloorK :: forall mx. SNatI mx => Natural -> Maybe (SomeFloorK mx)
mkSomeFloorK cur = reify (fromNatural cur)  (fmap MkSomeFloorK . toMaybeFloor)
  where 
    toMaybeFloor :: forall {flr :: Nat}. SNatI flr => Proxy flr -> Maybe (FloorK mx flr)
    toMaybeFloor (_p :: Proxy flr) = mkFloorK 

callSome :: MonadIO m
         => SomeFloorK mx -> SomeElevatorK mx -> m (SomeElevatorK mx)    
callSome (MkSomeFloorK flr)  (MkSomeElevatorK elev) = 
  MkSomeElevatorK <$> call flr elev  
  
-- >>>:t mkSomeFloorK 5  
-- mkSomeFloorK 5 :: SNatI mx => Maybe (SomeFloorK mx)

-- >>>:t mkFloorK @Nat5 @Nat3
-- mkFloorK @Nat5 @Nat3 :: Maybe (FloorK Nat5 Nat3)

-- >>>:t MkFloorK @Nat5 @Nat4
-- MkFloorK @Nat5 @Nat4 :: FloorK Nat5 Nat4

-- >>>reify (fromNatural 3) reflect
-- 3

-- >>>:i Proxy
-- type role Proxy phantom
-- type Proxy :: forall {k}. k -> *
-- data Proxy t = Proxy
--   	-- Defined in ‘Data.Proxy’

-- >>>:i reify
-- reify :: Nat -> (forall (n :: Nat). SNatI n => Proxy n -> r) -> r
--   	-- Defined in ‘Data.Type.Nat’

-- >>>:i fromNatural
-- fromNatural :: Natural -> Nat 	-- Defined in ‘Data.Nat’

-- >>>:t fromNatural 56
-- fromNatural 56 :: Nat

-- >>> explicitShow (fromNatural 11)
-- "S (S (S (S (S (S (S (S (S (S (S Z))))))))))"

-- >>>:i Natural
-- type Natural :: *
-- data Natural = NS Word# | NB ByteArray#
--   	-- Defined in ‘GHC.Num.Natural’

-- >>>:i Nat
-- type Nat :: *
-- data Nat = Z | S Nat
--   	-- Defined in ‘Data.Nat’

-- >>>:t (MkFloorK @Nat5 @Nat3)
-- (MkFloorK @Nat5 @Nat3) :: FloorK Nat5 Nat3