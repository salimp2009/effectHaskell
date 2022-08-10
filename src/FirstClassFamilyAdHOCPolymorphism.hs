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
import GHC.TypeLits (type (+), Nat, AppendSymbol)
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

-- >>>:kind! Evaltf (Mappend (Eq Int) (Evaltf (MEmpty (Ord Int))) )
-- Evaltf (Mappend (Eq Int) (Evaltf (MEmpty (Ord Int))) ) :: Constraint
-- = (Eq Int, () :: Constraint)

-- >>>:kind! MEmpty (Ord Int)
-- MEmpty (Ord Int) :: Constraint -> *
-- = MEmpty (Ord Int)
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

type instance (<>) '(a1, a2) '(b1, b2) = '(a1 <> b1 , a2 <> b2)

type instance (<>) _a _b = '()

type instance (<>) 'EQ b = b
type instance (<>) a 'EQ = a
type instance (<>) 'LT _b = 'LT
type instance (<>) 'GT _b = 'GT

type instance (<>) _1 'Nothing = _1
type instance (<>) 'Nothing _1 = _1
type instance (<>) ('Just a)  ('Just b) = 'Just (a <> b)

type instance (<>) (a::Nat) (b::Nat) = a + b
type instance (<>) x y = AppendSymbol x y

-- >>>:kind! Evaltf ('[1,2,3] ++ '[4, 5, 6])
-- Evaltf ('[1,2,3] ++ '[4, 5, 6]) :: [Nat]
-- = '[1, 2, 3, 4, 5, 6]
data (++) :: [a] -> [a] -> Exp [a]
type instance Evaltf ((++) xs ys) = xs <> ys

-- | This is the alternative but it (++) becomes general
-- to List; the above version is specific to List
-- Check if it is any difference 
-- data (++) :: a -> a -> Exp a
-- type instance Evaltf ((++) (xs::[k]) (ys::[k])) = xs <> ys


-- | given a type of any
-- monoidal kind K, Mempty will give back the monoidal
-- identity for that kind
{- 
  "it’s unclear how the approach
  can be used to implement Mempty. Type families are not
  allowed to discriminate on their return type. We can cheat
  this restriction by muddying up the interface a little and
  making the “type application” explicit."
-}

-- >>>:k Constraint
-- Constraint :: *

-- >>>:kind! Evaltf (MEmpty (Eq Int))
-- Evaltf (MEmpty (Eq Int)) :: Constraint
-- = () :: Constraint
data MEmpty :: k -> Exp k
--type instance Evaltf (MEmpty (MEmpty_ :: k))  =  MEmpty_::k
type instance Evaltf (MEmpty '()) = '()
type instance Evaltf  (MEmpty (c::Constraint)) = (()::Constraint)
type instance Evaltf (MEmpty (l::[k])) = '[]

-- >>>:kind! MEmpty_ <> '[1, 2, 3]
-- MEmpty_ <> '[1, 2, 3] :: [Nat]
-- = '[1, 2, 3]

-- | this did not work correctly for Tuples ???
-- >>>:kind! '( '[1], '[2]) <> MEmpty_
-- '( '[1], '[2]) <> MEmpty_ :: ([Nat], [Nat])
-- = '( '[1], '[2])

-- >>>:kind! '( 1, 2) <> MEmpty_
-- '( 1, 2) <> MEmpty_ :: (Nat, Nat)
-- = '(1, 2)

-- >>>:kind! '( 'GT, 'Just '()) <> MEmpty_
-- '( 'GT, 'Just '()) <> MEmpty_ :: (Ordering, Maybe ())
-- = '( 'GT, 'Just '())

-- >>>:kind! '( "salitos ", "didos") <> MEmpty_
-- '( "salitos ", "didos") <> MEmpty_ :: (Symbol, Symbol)
-- = '("salitos ", "didos")
type family MEmpty_ :: a
type instance MEmpty_  = '[]
type instance MEmpty_  = 'Nothing
type instance MEmpty_  = '()
type instance MEmpty_  = ()::Constraint
type instance MEmpty_  = ""
type instance MEmpty_  = '(MEmpty_ , MEmpty_)
type instance MEmpty_  = '(MEmpty_ , MEmpty_ , MEmpty_)
type instance MEmpty_  = 'EQ
type instance MEmpty_  = 0::Nat

