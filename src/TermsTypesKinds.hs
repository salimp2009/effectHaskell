{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- {-# LANGUAGE KindSignatures #-}  -- Not needed when used PolyKinds
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-} -- Needed for Type application example 
{-# LANGUAGE TypeApplications #-}

module TermsTypesKinds where

import Data.Proxy (Proxy(..))
import Data.Kind (Type)

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
-- after the extension  KindSignatures and import Data.Kind (Type)   
-- we are specifically show the type of u as u::Type
-- Original               -> class UnitName u where
-- after Data.Kind (Type) -> class UnitName (u::Type) where
-- after {-# LANGUAGE PolyKinds #-} extension we are allowed to use Kinds
-- e.g: Term:: Type -> Type    
class UnitName (u::k) where
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
    -- unitName :: forall k (u :: k). UnitName u => Proxy u -> String  -- ^ after the change of UnitName def changed
    -- unitName :: Proxy (Temp unit) -> String
    unitName _ = unitName (Proxy::Proxy unit)

instance UnitName unit => Show (Temp unit) where
    show (Temp t)= show t <> "°" <> unitName (Proxy::Proxy unit)

data K

instance UnitName K where
    unitName :: Proxy K -> String
    unitName _ = "K"

-- | use case; >unit (10::Temp F) -> "F"
unit :: forall u. UnitName u => Temp u -> String
unit _ = unitName (Proxy::Proxy u)

-- | Originally UnitName expects a Type but Temp is Type -> Type
-- after {-# LANGUAGE PolyKinds #-} extension we can use Kinds as Types
-- because type classes are not Polymorphic by default
instance UnitName Temp where
    unitName :: forall k (u :: k). UnitName u => Proxy u -> String
    unitName _ = "_unspecified unit_ !!"


-- | Type Applications
-- need to use {-# LANGUAGE AllowAmbiguousTypes #-}
class UnitName' u where 
    unitName' :: String

-- | use case ; unitName' @C -> "C"
instance UnitName' C where
    unitName' = "C"

-- | use case ; unitName' @F -> "F"
instance UnitName' F where
    unitName' = "F"

-- | use case; unitName' @Temp -> "unspecified unit!!"
instance UnitName' Temp where
    unitName' = "unspecified unit!!"

-- | use case ; unitName' @(Temp F) -> "F"
instance UnitName' u => UnitName' (Temp u) where
    unitName' = unitName' @u

-- | created this to make the below Show instance work
newtype Temp' unit = Temp' Double
    deriving (Num, Fractional)

-- | this conflicts with previous Show instance for Temp u
-- therefore created Temp'; now both works
-- usecase ; 10::Temp' F -> 10.0°F 
-- this works too ; 10::Temp F -> 10.0°F 
instance UnitName' u => Show (Temp' u) where
    show (Temp' t) = show t <> "°" <> unitName' @u

-- | use case: unit' (10::Temp' F) -> "F"
unit' :: forall u. UnitName' u => Temp' u -> String
unit' _ = unitName' @u



