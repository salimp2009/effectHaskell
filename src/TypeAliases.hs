module TypeAliases where

-- | Example of how not to use Type aliases

velocity :: Double -> Double -> Double
velocity meters seconds = meters / seconds

gravity :: Double
gravity = 
    let 
        meters = 9.8 :: Double
        seconds = 1.0 :: Double
    in velocity meters seconds 