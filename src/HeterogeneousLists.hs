{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE UndecidableInstances #-}
module HeterogeneousLists where
    
import Data.Kind (Constraint, Type)

-- | primary goal to build a list to store different types
-- we use GADTs to
-- define a heterogeneous list—a list which can store values
-- of different types inside it

-- One of the primary motivations of GADTs is building
-- inductive type-level structures out of term-level data

-- the 


-- use cases;
-- >>>:t True :# HNil
-- True :# HNil :: HList '[Bool]

-- >>> let hList = Just "hello" :# True :# HNil
-- >>>:t hList
-- hList :: HList '[Maybe String, Bool]

-- >>>hLength hList
-- 2
data HList (ts :: [Type]) where
    HNil :: HList '[]
    (:#) :: t -> HList ts -> HList (t ': ts)
infixr 5 :#

-- instance Eq (HList '[]) where
--     HNil == HNil = True

-- instance (Eq t, Eq (HList ts)) => Eq (HList (t ': ts)) where
--     (a :# as) == (b :# bs) = a == b && as == bs

-- instance Ord (HList '[]) where
--     compare HNil HNil = EQ

-- instance (Ord t, Ord (HList ts)) => Ord (HList (t ': ts)) where
--     compare (a :# as) (b :# bs) = compare a b <> compare as bs

-- instance Show (HList '[]) where
--     show HNil= "HNil"

-- instance (Show t, Show (HList ts)) 
--         => Show (HList (t ': ts)) where 
--     show (a :# as) = show a <> " :# " <> show as

-- | creating Equality instance that covers both a plain type t and HList ts
-- the empty HList is defined as unit () and type family returns it as Constraint type
-- for the HList ts case e define a tuple of Constraint list, that recursively creates 
-- Eq constraint for each type in the list 
-- so instead of defining a Eq instance seperately we define a type family that returns a constraint
-- so we can use that instead
-- check in repl;
-- >>>:kind! AllEq '[Int, Bool]   
-- AllEq '[Int, Bool] :: Constraint
-- = (Eq Int, (Eq Bool, () :: Constraint))
type family AllEq (ts :: [Type])::Constraint where
    AllEq '[]  = ()
    AllEq (t ': ts)  = (Eq t, AllEq ts)  

-- | we generalize the constraint type input c :: Type -> Constraint
-- so we can create instances of Constraint by using All otherwise
-- if we use AllEq it only works for Eq and we have to create another for Show or Ord
-- this way we create only instance of constraint using All   
-- if this is defined, we need to disable other instance of Eq, Ord we have created above otherwise they overlap     
type family All (c:: Type -> Constraint) (ts::[Type]) :: Constraint  where
    All c '[] = ()
    All c (t ': ts) = (c t, All c ts)

instance All Eq ts => Eq (HList ts) where
    HNil      == HNil      = True
    (a :# as) == (b :# bs) = a == b && (as == bs)

-- | Ord instance requires ts to Eq instance in the definition
-- to compare the types of 2 lists needs to match the value can be different 
-- >>>let hlist1 = 1 :# True :# False :# "salitos" :# HNil
-- >>>let hlist2 = 1 :# True :# False :# "didos":# HNil
-- >>>hlist1 > hlist2
-- >>>hlist1 == hlist2
-- True
-- False

instance (All Ord ts, All Eq ts) => Ord (HList ts) where
    compare HNil HNil           = EQ
    compare (a :# as) (b :# bs) = compare a b <> compare as bs

-- | use case;
-- >>>1 :# True :# False :# "salitos" :# HNil    
-- 1:# True:# False:# "salitos":# HNil
-- >>>:t it
-- it :: Num t => HList '[t, Bool, Bool, String]

instance (All Show ts) => Show (HList ts) where
    show HNil      = "HNil"
    show (a :# as) = show a <> ":# " <> show as

hLength :: HList ts -> Int
hLength HNil      = 0
hLength (_ :# ts) = 1 + hLength ts

hHead :: HList (t ':ts) -> t
hHead (t :# _) = t

showBool :: HList '[ _1, Bool , _3 ] -> String
showBool (_ :# b :# _ :# HNil) = show b





