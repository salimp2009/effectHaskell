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
    | otherwise = error "Elevator is at Ground floor"

down el@(Elevator _ Opened) = error "door is open, please close the door!"

up :: MonadIO m => Elevator -> m Elevator
up el@(Elevator fl@(Floor n) Closed)
    | belowTop fl = do
        liftIO $ LL.up
        pure $ el {current = Floor (n+1)}
    | otherwise = error "Elevator is at Top floor"
up el@(Elevator _ Opened) = error "door is open, please close the door!"

open :: MonadIO m => Floor -> Elevator -> m Elevator
open fl el 
    | sameFloor fl el =
        if isClosed el
        then do            
            liftIO $ LL.open
            pure $ el {door = Opened}
        else
            error "Door is already opened"
    | otherwise = error 
            "Can't open door when we are not on the right floor or moving"

close :: MonadIO m => Floor -> Elevator -> m Elevator
close fl el
    | sameFloor fl el =
        if isOpened el
            then do 
                liftIO $ LL.close
                pure $ el {door = Closed}
            else
                error "Door is already closed"
    | otherwise = error "Can't close when we are on the right floor or moving"

ensureClosed :: MonadIO m => Elevator -> m Elevator            
ensureClosed el 
        | isClosed el  = pure el
        | otherwise    = close (current el) el

moveTo :: MonadIO m => Floor -> Elevator -> m Elevator
moveTo fl el' = do
          el <- ensureClosed el'
          case compare fl (current el) of
            EQ -> pure el 
            GT -> up el >>= moveTo fl
            LT -> down el >>= moveTo fl

call :: MonadIO m => Floor -> Elevator -> m Elevator
call fl el = do
    liftIO $ putStrLn $ "Call to: " <> show fl
    if sameFloor fl el
       then (if isOpened el then pure el else open fl el)
       else moveTo fl el >>= open fl

-- Alternative implementation
    -- case compare fl (current el) of
    --     EQ -> if isOpened el 
    --           then pure el 
    --           else open fl el
    --     _ -> moveTo fl el >>= open fl
