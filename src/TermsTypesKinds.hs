{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module TermsTypesKinds where

-- example for temperature and using phantom types
-- unit is a phantom type but it has a flaw cause
newtype Temp unit = Temp Double
    deriving (Num, Fractional, Show)

-- types to use as unit in Temp
-- no value
data F
data C

paperBurning :: Temp F
paperBurning = 451

absoluteZero :: Temp C
absoluteZero = -273.15

fahrenheitToCelcius :: Temp F -> Temp C
fahrenheitToCelcius (Temp f) = Temp ((f-32) * 5 / 9)

-- no control over unit
-- therefore below code compiles
-- unit == Bool but Temp value is still double
nonsense :: Temp Bool
nonsense = 0
