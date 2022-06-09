{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
--{-# LANGUAGE ScopedTypeVariables #-}

module DataKindsExample where

-- | with DataKinds extension TempUnits is promoted to kind
-- F' and C' are promoted to types
data TempUnits = F' | C'

-- | need to have {-# LANGUAGE DataKinds #-} extension
-- Tempr is a new parameterized type with a variable limited
-- to a specified kind ; in this e.g. TEmpUnits kind
-- so the only possible types are F' and C'
newtype Tempr (u::TempUnits) = Tempr Double
    deriving (Num, Fractional, Show)

paperBurning' :: Tempr F'
paperBurning' = 451

absoluteZero' :: Tempr C'
absoluteZero' = -273.15

fahrenToCelcius :: Tempr F' -> Tempr C'
fahrenToCelcius (Tempr f) = Tempr ((f-32) * 5/9)

valx :: Maybe Int
valx = Just 20