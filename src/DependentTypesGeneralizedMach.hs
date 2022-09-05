{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
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

  