{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
--{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module GeneralConstraintKindExistentials where

import Data.Kind (Type)
import GHC.Base (Constraint)
import Type.Reflection (Typeable(..))

-- | example to show making constraint POlymorphic
-- that are used in ExistentialTypes module
-- define HasShow as GADt; 
-- HasShow :: Show t => t -> HasShow
data Has (c:: Type -> Constraint) where
  Has :: c t => t -> Has c 
 
elimHas :: (forall a . c a => a -> r)  -> Has c -> r
elimHas f (Has a) =  f a

-- | creating type synonyms using polymorphic
-- constraint kinds with existential types
type HasShow2 = Has Show
type Dynamic3 = Has Typeable

-- | multpile constraints
isMempty :: (Monoid a, Eq a) => a -> Bool
isMempty a = a == mempty

-- | using multiple constraints to create Has instance
-- dont work this way
-- >>>:t Has [True] :: Has MonoidAndEq
-- The type synonym ‘MonoidAndEq’ should have 1 argument, but has been given none

-- >>>:t Has [True] :: Has Show
-- Has [True] :: Has Show :: Has Show
type MonoidAndEq a = (Monoid a, Eq a)

-- | solution for constraint synonyms ; 
-- (Not sure if this is the best way); 
-- new class with superclass constraint
-- with a polymorphic instance ; known a constraint synonym
-- type synonmys cannot be partiall applied classes has no such
-- restriction with extensions; 
-- {-# LANGUAGE FlexibleInstances #-} 
-- {-# LANGUAGE UndecidableInstances #-}

-- use case; 
--  >>>let foo = Has [ True ] :: Has MonoidEq
-- >>>elimHas isMempty foo
-- False

--  >>>let foo2 = Has [ ] :: Has MonoidEq
-- >>>elimHas isMempty foo2
-- True

--  >>>let foo3 = Has (Just [4]) :: Has MonoidEq
-- >>>elimHas isMempty foo3
-- False

--  >>>let foo4 = Has (Nothing) :: Has MonoidEq
-- >>>elimHas isMempty foo4
-- True

-- >>>:t Has [ True ] :: Has MonoidEq
-- Has [ True ] :: Has MonoidEq :: Has MonoidEq
class (Monoid a, Eq a) => MonoidEq a
instance (Monoid a, Eq a) => MonoidEq a
