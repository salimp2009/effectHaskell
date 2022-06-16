module TypeAliases where

-- | Example of how not to use Type aliases
type Meters  = Double
type Seconds = Double
type MetersPerSecond = Double

velocity :: Meters -> Seconds -> MetersPerSecond
velocity meters seconds = meters / seconds

gravity :: Double
gravity = 
    let 
        meters = 9.8 :: Meters
        seconds = 1.0 :: Seconds
    in velocity meters seconds 

-- | Right ways to use type aliases
-- type alias is used to apply a type to a partially applied type
-- e.g : String' applies Char to NewList and creates NewList Char
data NewList a = EmptyNew | NewList a
type String' = NewList Char
