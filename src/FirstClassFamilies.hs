{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}

module FirstClassFamilies where

import TypeLevelDefunctionalization (Exp, Evaltf, Snd, FromMaybe, MapListt)
import Data.Kind (Type, Constraint)
import GHC.TypeLits (type (+), Nat)
-- import Data.Type.Equality (type (==))
-- import Data.Type.Bool (type (&&))

-- | First Class families form a monad a t type level

-- >>>:kind! Evaltf (Pure 5)
-- Evaltf (Pure 5) :: Nat
-- = 5
data Pure :: a -> Exp a 
type instance Evaltf (Pure x) = x


data (=<<) :: (a -> Exp b) -> Exp a -> Exp b
infixr 0 =<<
type instance Evaltf (f =<< g) = Evaltf (f (Evaltf g ))

-- | to compose; the traditional Fish operator (<=<) ; Kleisli composition
-- is used to represent this combinator.
-- but at type level we can not use the the dot operator 
-- since it conflicts with forall quantifier
type Snd2 = Snd <=< Snd

-- >>>:kind! Evaltf (Snd2 '(1, '(2, 3)))
-- Evaltf (Snd2 '(1, '(2, 3))) :: Nat
-- = 3

-- >>>:kind! Evaltf (Snd2 '(1, '(2, "salitos")))
-- Evaltf (Snd2 '(1, '(2, "salitos"))) :: Symbol
-- = "salitos"

-- >>>:kind! Evaltf (Snd <=< FromMaybe '(0, 0) =<< Pure (Just '(1, 2)))  
-- Evaltf (Snd <=< FromMaybe '(0, 0) =<< Pure (Just '(1, 2))) :: Nat
-- = 2

--- >>>:kind! Evaltf (Snd <=< FromMaybe '(0, "salikos") =<< Pure Nothing) 
-- Evaltf (Snd <=< FromMaybe '(0, "salikos") =<< Pure Nothing) :: Symbol
-- = "salikos"
data (<=<) :: (b -> Exp c) -> (a -> Exp b) -> a -> Exp c
infixr 1 <=<
type instance Evaltf ((f <=< g) x) = Evaltf (f (Evaltf (g x)))


data TypeEq :: a -> a -> Exp Bool
type instance Evaltf (TypeEq x y) = TyEqImpl x y

type family TyEqImpl (a::k) (b::k) :: Bool where
  TyEqImpl a a = 'True
  TyEqImpl a b = 'False

  -- | use case of Alltf to apply a given Constraint to all types in a type List
  -- it returns a tuple Constraints
-- >>>:kind! Evaltf (Alltf Eq '[Int, Bool] )  
-- Evaltf (Alltf Eq '[Int, Bool] ) :: Constraint
-- = (Eq Int, (Eq Bool, () :: Constraint))
data Collapse :: [Constraint]  -> Exp Constraint
type instance Evaltf (Collapse '[]) = (()::Constraint)
type instance Evaltf (Collapse ( a ': as)) = (a, Evaltf (Collapse as))
 
-- | data MapListt :: (a -> Exp b) -> [a] -> Exp [b]
-- Pure1 applies given function to given input return result as Exp
-- MapLisst takes a function and applies the function all elems returns
-- Exp [b]
-- =<< take a Exp (the result of MapListt) and passes the
-- list [b] inside the Exp [b] to given to given function 
-- Collapse which takes a list return us a tuple of Constraints
type Alltf (c:: k -> Constraint) (ts::[k]) = 
    Collapse =<< MapListt (Pure1 c) ts

data Pure1 :: (a -> b) -> a -> Exp b
type instance Evaltf (Pure1 f x) = f x       
