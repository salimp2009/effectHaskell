{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
module TypeLevelDefunctionalization where
{- from "Thinking With Types book" by Sandy Maguire
  "This entire line of reasoning lifts, as Xia shows, to the
  type-level where it fits a little more naturally. Because
  type families are capable of discriminating on types, we
  can write a defunctionalized symbol whose type
  corresponds to the desired type-level function.
  These are known as first class families, or FCFs for short"
-}  

import Data.Kind(Constraint, Type)
import GHC.TypeLits (type (+), Nat)

-- | kind synonym, Exp a
-- describing a type level function
-- will produce kind A
type Exp a = a -> Type

-- | Open type family Eval
-- Eval matches on any Exp a mapp them to to an kind A
type family Evaltf (e :: Exp a) :: a

-- | to write defunctionalize labels
-- empty data-types will be used
-- snd function to type-level , we write 
-- a data type whose kind mirrors of the function snd
-- >>>:t snd
-- snd :: (a, b) -> b

-- >>>:set -XNoStarIsType
-- >>>:k Snd
-- Snd :: (a, b) -> b -> Type
data Snd :: (a, b) -> Exp b
-- >>>:kind! Evaltf (Snd '(True, "hello"))
-- Evaltf (Snd '(True, "hello")) :: Symbol
-- = "hello"
type instance Evaltf (Snd '(a, b)) = b

-- | function that perform pattern matching
-- can be lifted to defunctionalize style by having
-- multiple instance for Evaltf
data FromMaybe :: a -> Maybe a -> Exp a
-- >>>:kind! Evaltf (FromMaybe "nada" ('Just "just right"))
-- Evaltf (FromMaybe "nada" ('Just "just right")) :: Symbol
-- = "just right"


-- >>>:kind! Evaltf (FromMaybe "nada" 'Nothing)
-- Evaltf (FromMaybe "nada" 'Nothing) :: Symbol
-- = "nada"
type instance Evaltf (FromMaybe _1 ('Just a)) = a
type instance Evaltf (FromMaybe a 'Nothing)   = a

data ListToMaybe :: [a] -> Exp (Maybe a)
-- >>>:kind! Evaltf (ListToMaybe  '[ 1 , 2 ] )
-- Evaltf (ListToMaybe  '[ 1 , 2 ] ) :: Maybe Nat
-- = 'Just 1

-- >>>:kind! Evaltf (ListToMaybe  '[ ] ) 
-- Evaltf (ListToMaybe  '[ ] ) :: Maybe a
-- = 'Nothing
type instance Evaltf (ListToMaybe (a ': _1) ) = 'Just a
type instance Evaltf (ListToMaybe '[] )      = 'Nothing


data MapListt :: (a -> Exp b) -> [a] -> Exp [b]

-- >>>:kind! Evaltf (MapListt (FromMaybe 0) '[ ( 'Just 1) ,  'Nothing])
-- Evaltf (MapListt (FromMaybe 0) '[ ( 'Just 1) ,  'Nothing]) :: [Nat]
-- = '[1, 0]

-- >>>:kind! Evaltf (MapListt (FromMaybe 0) '[ ])  
-- Evaltf (MapListt (FromMaybe 0) '[ ]) :: [Nat]
-- = '[]
type instance Evaltf (MapListt f (a ': as)) = Evaltf (f a) ': Evaltf (MapListt f as)
type instance Evaltf (MapListt f '[])       = '[]

data Foldr :: (a -> b -> Exp b) -> b -> [a] -> Exp b

data Sum :: a -> a -> Exp a
type instance Evaltf (Sum x y ) = x + y

-- >>>:kind! Evaltf(Foldr Sum 0 '[1, 2, 3])
-- Evaltf(Foldr Sum 0 '[1, 2, 3]) :: Nat
-- = 6
type instance Evaltf (Foldr _1 b '[]) = b
type instance Evaltf (Foldr f b (a ': as)) = Evaltf (f a (Evaltf (Foldr f b as)))
