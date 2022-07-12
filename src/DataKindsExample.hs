{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PolyKinds #-}
--{-# LANGUAGE InstanceSigs #-}
--{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module DataKindsExample where
-- import Data.Kind (Type)

-- | with DataKinds extension TempUnits is promoted to kind
-- F' and C' are promoted data constructors in the same namespace types 
-- F' and C' can only be used at type level (cannot used in the runtime)
data TempUnits = F' | C'

-- | need to have {-# LANGUAGE DataKinds #-} extension
-- Tempr is a new parameterized type with a variable limited
-- to a specified kind ; in this e.g. TEmpUnits kind
-- so the only possible types are F' and C'
newtype Tempr (u::TempUnits) = Tempr Double
    deriving (Num, Fractional)

paperBurning' :: Tempr F'
paperBurning' = 451

absoluteZero' :: Tempr C'
absoluteZero' = -273.15

fahrenToCelcius :: Tempr F' -> Tempr C'
fahrenToCelcius (Tempr f) = Tempr ((f-32) * 5/9)

class UnitNameR (u::TempUnits) where
    unitNameR :: String

instance UnitNameR F' where
    unitNameR = "F'"

instance UnitNameR C' where
    unitNameR = "C'"

-- this is not allowed even with PolyKinds    
-- instance UnitNameR Tempr  where
--     unitNameR = "unspecified unit"

-- | use case ;
-- unitR (10::Tempr F') -> "F'"
unitR :: forall u. UnitNameR u => Tempr u -> String
unitR _ = unitNameR @u

-- | use case;
--10::Tempr F' -> 10.0°F'
instance UnitNameR u => Show (Tempr u) where
    show (Tempr t) = show t <> "°" <> unitNameR @u