{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
module FirstClassFamilyAdHOCPolymorphism where

import TypeLevelDefunctionalization (Exp, Evaltf, Snd, Sum)
import Data.Kind (Type, Constraint)
import GHC.TypeLits (type (+), Nat)
import Data.Monoid ((<>))



-- | use case for Map at type level
-- >>>:kind! Evaltf (Mapt Snd ('Just '(1, 2)))
-- Evaltf (Mapt Snd ('Just '(1, 2))) :: Maybe Nat
-- = 'Just 2

-- >>>:kind! Evaltf (Mapt (Mapt Snd) '[ ('Just '(1, 2)), ('Just '(3, 4))] )
-- Evaltf (Mapt (Mapt Snd) '[ ('Just '(1, 2)), ('Just '(3, 4))] ) :: [Maybe
--                                                                      Nat]
-- = '[ 'Just 2, 'Just 4]

-- >>>:kind! Evaltf (Mapt Snd ('Left '(1, 2)))
-- Evaltf (Mapt Snd ('Left '(1, 2))) :: Either (Nat, Nat) b
-- = 'Left '(1, 2)

-- >>>:kind! Evaltf (Mapt Snd ('Right '(1, 2)))
-- Evaltf (Mapt Snd ('Right '(1, 2))) :: Either a Nat
-- = 'Right 2

-- >>>:kind! Evaltf (Mapt Snd ('Left 'False))
-- Evaltf (Mapt Snd ('Left 'False)) :: Either Bool b
-- = 'Left 'False

-- >>>:kind! Evaltf (Mapt Snd ('Right '( 'False, 'True)))
-- Evaltf (Mapt Snd ('Right '( 'False, 'True))) :: Either a Bool
-- = 'Right 'True
-- | promoted functor (Mapt) instance of []
data Mapt :: (a -> Exp b) -> f a -> Exp (f b)
type instance Evaltf (Mapt f '[]) = '[]
type instance Evaltf (Mapt f (a ': as)) = Evaltf (f a) ': Evaltf (Mapt f as)

-- | promoted functor (Mapt) instance of Maybe a
type instance Evaltf (Mapt f 'Nothing)  = 'Nothing 
type instance Evaltf (Mapt f ('Just a)) = 'Just (Evaltf ( f a))

type instance Evaltf (Mapt f ('Left a) )  = 'Left a
type instance Evaltf (Mapt f ('Right b) ) = 'Right (Evaltf (f b))

-- | promoted functor (Mapt) instance  of Tuple
-- >>>:kind! Evaltf (Mapt (Sum 1) '(1, 2))
-- Evaltf (Mapt (Sum 1) '(1, 2)) :: (Nat, Nat)
-- = '(1, 3)
type instance Evaltf (Mapt f '(a , b)) = '(a, Evaltf (f b))

-- | Semigroup Operation Mappend implementation
-- using ad-hoc polymorphisms at type-level with First Class Families
-- >>>:kind! Evaltf (Mappend '[2, 3] '[3, 4])
-- Evaltf (Mappend '[2, 3] '[3, 4]) :: [Nat]
-- = '[2, 3, 3, 4]

-- >>>:kind! Evaltf (Mappend '["sal", "monoidos"] '["semos", "didokitos"])
-- Evaltf (Mappend '["sal", "monoidos"] '["semos", "didokitos"]) :: [Symbol]
-- = '["sal", "monoidos", "semos", "didokitos"]

-- >>>:kind! Evaltf (Mappend (Eq Int) (Ord Int))
-- Evaltf (Mappend (Eq Int) (Ord Int)) :: Constraint
-- = (Eq Int, Ord Int)
data Mappend :: a -> a -> Exp a
type instance Evaltf (Mappend '() '()) = '()
type instance Evaltf (Mappend (a::Constraint) (b::Constraint)) = (a, b)
type instance Evaltf (Mappend (as::[k]) (bs::[k])) = Evaltf (as ++ bs)


-- >>>:kind! ([1,2,3] <> [4, 5, 6])
-- ([1,2,3] <> [4, 5, 6]) :: [Nat]
-- = '[1, 2, 3, 4, 5, 6]
type family (<>) (x :: a) (y :: a)::a
type instance (<>) '[] ys = ys
type instance (<>) (x ': xs) ys = x ': (<>) xs ys

-- >>>:kind! Evaltf ('[1,2,3] ++ '[4, 5, 6])
-- Evaltf ('[1,2,3] ++ '[4, 5, 6]) :: [Nat]
-- = '[1, 2, 3, 4, 5, 6]
data (++) :: [a] -> [a] -> Exp [a]
type instance Evaltf ((++) xs ys ) = xs <> ys

-- | this is defined in the book but does not work because (++) operator
-- not defined level
--type instance Evaltf (Mappend '[] (bs::[k])) = bs
--type instance Evaltf (Mappend ((a ': as)::[k]) (bs::[k])) = a ': Evaltf (Mappend as bs) 
