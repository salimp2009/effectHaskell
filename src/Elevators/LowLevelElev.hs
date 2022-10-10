module Elevators.LowLevelElev where

-- | simulating an elevator low level operation
-- but not safe because there is  no check for doors or levels
-- we cant move when the door is open or go beyon ground level or parking
-- cant go above the top floor; there is no check in this version
up :: IO()
up = putStrLn "Going up"

down :: IO()
down = putStrLn "Going down"

open :: IO ()
open = putStrLn "Door is opening"

close :: IO ()
close = putStrLn "Door is closing"
  