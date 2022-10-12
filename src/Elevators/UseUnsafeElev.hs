module Elevators.UseUnsafeElev where

import Control.Monad
import System.Environment
import Elevators.UnsafeElev
import Text.Read (readMaybe)
  
gfElevator :: Elevator 
gfElevator = Elevator (Floor 0) Closed


useElev :: IO ()
useElev = do
  -- print "please enter floor nums: "
  floors <- map read <$> getArgs
  foldM_ traceTo gfElevator (map Floor floors)
      where
          prt el = print el >> pure el
          traceTo el fl = call fl el >>= prt

