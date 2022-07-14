-- {-# LANGUAGE DefaultSignatures #-}
-- {-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
-- {-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}

module GenericEquality where
import GHC.Generics

-- | Generic equality will serve as a carrier  
-- to Generically implement Eq as an exercise
-- otherwise Eq can be written by the compiler
-- geq method signature is close to (==) :: a -> a -> Bool
-- but Rep has a signature Type -> Type to deal with higher-kinded classes
-- therefore we need to saturate type of kind Type by using a dummy type x 
-- to make it kind check
-- Check if Generics1 can be use since it use kind -> Type
class GEq a where
    geq :: a x -> a x -> Bool



-- | Write Generic instances starting from inner to out
-- constructors U1, V1, K1
-- U1 is constructor with no value which is equal to ()
instance GEq U1 where
    geq U1 U1 = True

  
-- | V1 is the type thast cannot be constructed
-- V1 is generic rep of Void ; which has no inhabitants
-- since it cant be constructed, it cant be tested therefore 
-- there is no harm to call True
instance GEq V1 where
    geq _ _ = True  

-- | concrete types inside the data const such as (Maybe a)
-- are represented with K1
instance Eq a => GEq (K1 _1 a ) where
    geq (K1 a) (K1 b) = a == b    
    
-- | lifting base cases to create instance for sum types 
-- L1 is the left (first) constructor of a sum type
-- R1 is the right (second) constructor of a sum type
instance (GEq a, GEq b) => GEq (a :+: b) where
    geq (L1 a1) (L1 a2) = geq a1 a2  
    geq (R1 b1) (R1 b2) = geq b1 b2
    geq _ _             = False   

instance (GEq a, GEq b) => GEq (a :*: b ) where
    geq (a1 :*: b1) (a2 :*: b2) = geq a1 a2 && geq b1 b2
    
instance GEq a => GEq (M1 _x _y a) where
    geq (M1 a1) (M1 a2) = geq a1 a2 

-- | use cases ;
-- >>> genericEq "ghc.generics" "ghc.generic" 
-- False
-- >>> genericEq "ghc.generics" "ghc.generics" 
-- True
-- >>> genericEq (Just 2) (Just 4)
-- False
genericEq :: (Generic a, GEq (Rep a)) => a -> a -> Bool
genericEq a b = geq (from a)  (from b)

data Foo a b c=  F0 
          | F1 a
          | F2 b c 
          deriving (Generic)

-- >>>:t F2 2 4          
-- F2 2 4 :: (Num b, Num c) => Foo a b c
-- >>>from (F2 2 4)
-- M1 {unM1 = R1 (R1 (M1 {unM1 = M1 {unM1 = K1 {unK1 = 2}} :*: M1 {unM1 = K1 {unK1 = 4}}}))}

-- |use cases;
-- >>> (F1 2) == (F1 3)
-- False
-- >>> (F1 2) == (F2 2 4 )
-- False
-- >>> (F2 2 4) == (F2 2 4 )
-- True
instance (Eq a, Eq b, Eq c) => Eq (Foo a b c) where
    (==) = genericEq

-- >>>from F0
-- M1 {unM1 = L1 (M1 {unM1 = U1})}

-- >>>:kind! Rep (Maybe Int)    
-- Rep (Maybe Int) :: * -> *
-- = D1
--     ('MetaData "Maybe" "GHC.Maybe" "base" 'False)
--     (C1 ('MetaCons "Nothing" 'PrefixI 'False) U1
--      :+: C1
--            ('MetaCons "Just" 'PrefixI 'False)
--            (S1
--               ('MetaSel
--                  'Nothing 'NoSourceUnpackedness 'NoSourceStrictness 'DecidedLazy)
--               (Rec0 Int)))

-- >>> from (Just 2)
-- M1 {unM1 = R1 (M1 {unM1 = M1 {unM1 = K1 {unK1 = 2}}})}

-- >>> from (1)
-- Couldn't match type: Rep a0
--                with: Rep a
-- Expected: Rep a x
--   Actual: Rep a0 x
-- NB: ‘Rep’ is a non-injective type family
-- The type variable ‘a0’ is ambiguous

-- >>>:k K1
-- K1 :: * -> * -> k -> *
-- >>>:k U1
-- U1 :: k -> *
-- >>>:k V1
-- V1 :: k -> *

-- >>>:k M1
-- M1 :: * -> Meta -> (k -> *) -> k -> *

-- >>>:t M1
-- M1 :: f p -> M1 i c f p
