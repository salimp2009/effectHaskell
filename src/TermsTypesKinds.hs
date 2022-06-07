{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TermsTypesKinds where

import Data.Proxy (Proxy(..))

-- example for temperature and using phantom types; 
-- unit is a phantom type but it has a flaw cause
newtype Temp unit = Temp Double
    deriving (Num, Fractional)

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

-- | Example to use Proxy which uses a phantom type t but does not use the value
-- data Proxy t = Proxy

-- | Proxy is used to create different functionality for unitname 
-- depending on the type
class UnitName u where
    unitName :: Proxy u -> String

-- | type annotation for unitName is not neccessary since it is inferred
-- if used then have use the {-# LANGUAGE InstanceSigs #-} extension
instance UnitName C where
    unitName :: Proxy C -> String   
    unitName _ = "C"

instance UnitName F where
    unitName :: Proxy F -> String   
    unitName _  = "F"

-- | we refer to unit inside Temp unit
-- therefore we need extension {-# LANGUAGE ScopedTypeVariables #-}
instance UnitName unit => UnitName (Temp unit) where
    -- unitName :: forall u. UnitName u => Proxy u -> String
    -- unitName :: Proxy (Temp unit) -> String
    unitName _ = unitName (Proxy::Proxy unit)

instance UnitName unit => Show (Temp unit) where
    show (Temp t)= show t <> "°" <> unitName (Proxy::Proxy unit)

data K

instance UnitName K where
    unitName :: Proxy K -> String
    unitName _ = "K"

unit :: forall u. UnitName u => Temp u -> String
unit _ = unitName (Proxy::Proxy u)


