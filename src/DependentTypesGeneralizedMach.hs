--{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
--{-# LANGUAGE KindSignatures #-}
--{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
--{-# LANGUAGE PatternSynonyms #-}
--{-# LANGUAGE ViewPatterns #-}

module DependentTypesGeneralizedMach where

import Data.Kind (Type)
import Data.Typeable 
import Data.Void
import Unsafe.Coerce (unsafeCoerce)

-- | the goal is to generalize Singleton that
-- we used with the AdHOC Singleton method
-- to work with different types


-- | poly-kinded data family; provides injectivity
-- by allowing different data constructor for different instances
-- for different types; it is always open

-- | Begin with poly kinded open data family
-- responsbile for indexing the equivalents of SBool
data family Sing (a::k)


data SomeSing k where
  SomeSing :: Sing (a :: k) -> SomeSing k

withSomeSing :: SomeSing k -> (forall (a::k). Sing a -> r) -> r 
withSomeSing (SomeSing s) f = f s

-- | instead of free function for toSing and fromSing
-- package them in a type class
-- in fromSing function the type parameter k is used
-- both as a type and kind with -XTypeInType extension
class SingKind k where
  type Demote k = r | r -> k
  toSing :: Demote k -> SomeSing k
  fromSing :: Sing (a :: k) -> Demote k

  {- ^  
    " The associated type family Demote k is an
    implementation detail. It is almost always equal to k,
    except in cases when GHC already provides a type literal
    (for NAT and SYMBOL.) A type family dependency is added
    to Demote, allowing GHC to infer k from Demote k. "
  -}

data instance Sing (a::Bool) where
  STrue  :: Sing 'True
  SFalse :: Sing 'False

-- >>>withSomeSing (toSing True) fromSing 
-- True

-- >>>withSomeSing (toSing False) fromSing 
-- False

instance SingKind Bool where
  type Demote Bool = Bool
  toSing True  = SomeSing STrue
  toSing False = SomeSing SFalse
  fromSing STrue  = True
  fromSing SFalse = False

-- | Singletons are unique inhabitant of their types
-- and at the term level they are isomorphic with ()
-- There we can get a () very easily ??
class SingI (a::k) where
  sing :: Sing a

instance SingI 'True where
  sing = STrue
  
instance SingI 'False where
  sing = SFalse
  
data instance Sing (a:: Maybe k) where
  SJust     :: Sing (a::k) -> Sing ('Just a)
  SNothing  :: Sing 'Nothing

instance SingI a => SingI ('Just a) where
  sing = SJust sing

instance SingI 'Nothing where
  sing = SNothing  

instance (k ~ Demote k , SingKind k ) => SingKind (Maybe k) where  
  type Demote (Maybe k) = Maybe k
  toSing (Just a) = withSomeSing (toSing a) $ SomeSing . SJust
  toSing Nothing  = SomeSing SNothing
  fromSing (SJust a) = Just $ fromSing a
  fromSing SNothing  = Nothing

data instance Sing (a ::[k]) where
  SNil  :: Sing '[]
  SCons :: Sing (h ::k) -> Sing (t::[k]) -> Sing (h ': t)

instance SingI '[] where
  sing = SNil

instance (SingI h, SingI t) => SingI (h ': t)  where
 sing = SCons sing sing 

instance (k ~ Demote k, SingKind k) => SingKind [k] where
  type Demote [k] = [k]
  toSing []  = SomeSing SNil
  toSing (h : t) = withSomeSing (toSing h) $ \sh ->
                   withSomeSing (toSing t) $ \st ->
                    SomeSing $ SCons sh st 
  fromSing (SCons h t) =  fromSing h : fromSing t
  fromSing SNil = []

-- | these are generated automatically with singleton package
-- using Template Haskell singletons [d| ...|] function 
-- together with the above Sing, SingI, SomeSing..definitions
-- SDecide is nominal equality for singletons
-- checks if two Sing s are equal  
class SDecide k where
      (%~) :: Sing (a :: k)
            -> Sing (b :: k)
            -> Decision (a :~: b)

-- | definition of :~: from Typeable            
-- data a :~: b where
--   Refl :: a :~: a 

-- The type a -> Void at 1 is the Curry–Howard
-- encoding of logical false—because Void is uninhabited, it
-- can’t be constructed. Thus, a function that produces Void
-- must not be callable
data Decision a
    = Proved a
    | Disproved (a -> Void)

-- | free instance of SDecide at term level 
-- which can also be used with singleton  package    
-- since singleton package does not have these 
instance (Eq (Demote k), SingKind k) => SDecide k where
      a %~ b =
        if fromSing a == fromSing b
           then Proved $ unsafeCoerce Refl
           else Disproved $ const undefined

-- | type level instance of SDecide
instance SDecide Bool where
  STrue %~ STrue    = Proved Refl
  SFalse %~ SFalse  = Proved Refl
  _ %~ _ = Disproved $ const undefined
   
instance SDecide k => SDecide (Maybe k) where
  SJust a %~ SJust b    =  
    case a %~ b of
      Proved Refl  -> Proved Refl
      Disproved _  -> Disproved $ const undefined
  SNothing %~ SNothing  = Proved Refl 
  _ %~ _ = Disproved $ const undefined 
