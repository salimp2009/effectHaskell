--{-# LANGUAGE TypeApplications #-}
module Elevators.UnsafeElev where

import Control.Monad.Trans
import qualified Elevators.LowLevelElev as LL

data DoorState = Opened | Closed
    deriving (Eq, Show)

newtype Floor = Floor Int
    deriving (Eq, Ord, Show)    

-- >>>:i Bounded
-- type Bounded :: * -> Constraint
-- class Bounded a where
--   minBound :: a
--   maxBound :: a
--   {-# MINIMAL minBound, maxBound #-}
--   	-- Defined in ‘GHC.Enum’
-- | Limiting the Floor max and min levels
instance Bounded Floor where
  minBound = Floor 0
  maxBound = Floor 5   

data Elevator = Elevator 
            { current :: Floor
            , door    :: DoorState
            }
            deriving Show

sameFloor :: Floor -> Elevator -> Bool
sameFloor flr elv = flr == current elv            

isClosed :: Elevator -> Bool
isClosed elv = door elv == Closed

isOpened :: Elevator -> Bool
isOpened elv = door elv == Closed

belowTop :: Floor -> Bool
belowTop flr = flr < maxBound  

aboveGround :: Floor -> Bool
aboveGround flr = flr > minBound

down :: MonadIO m => Elevator -> m Elevator
down el@(Elevator fl@(Floor n) Closed) 
    | aboveGround fl = do
          liftIO $ LL.down
          pure $ el {current = Floor (n-1)}
    | otherwise = error "Elevator is on Ground floor"

down el@(Elevator _ Opened) = error " door is open, please close the door!"

open :: MonadIO m => Floor -> Elevator -> m Elevator
open fl el = undefined