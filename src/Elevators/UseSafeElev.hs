{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Elevators.UseSafeElev where

import Elevators.SafeElev

import Data.Type.Nat
import Control.Monad
import System.Environment
import Text.Read (readMaybe, read)

type MX = Nat5

groundfloorElevator :: ElevatorK MX Nat0 'ClosedK
groundfloorElevator = MkElevatorK (MkFloorK :: FloorK MX Nat0)

exampleK :: IO()
exampleK = pure groundfloorElevator >>= printElev
          >>= traceElev (MkFloorK :: FloorK MX Nat2)
          >>= traceElev (MkFloorK :: FloorK MX Nat2)
          >>= traceElev (MkFloorK :: FloorK MX Nat3)
          >>= traceElev (MkFloorK :: FloorK MX Nat5)
          >>= traceElev (MkFloorK :: FloorK MX Nat0) >> pure ()
    where
      printElev el = print el >> pure el
      traceElev flr el = call flr el >>= printElev

-- | below is a simulation 
-- if we are given a list of SomeFloorK MX values as a list of request
-- as if the elevator is called from different floors          
simulateElev :: [SomeFloorK MX] -> IO ()
simulateElev = foldM_ traceToSome (MkSomeElevatorK groundfloorElevator)
      where
        printElv el = print el >> pure el
        traceToSome el fl = callSome fl el >>= printElv

runSimulation :: IO ()
runSimulation = do
  let flrReqs = map show [0..5]
  --flrReqs <- getArgs
  let maybeFloors = mapM (mkSomeFloorK . read) flrReqs
  case maybeFloors of
    Nothing -> putStrLn "Incorrect floors"
    Just floors -> simulateElev floors

