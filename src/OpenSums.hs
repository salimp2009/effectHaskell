{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module OpenSums where 

{- |
  "an open sum is a container of a data
whose type isn’t known statically. 
  thereare no guarantees that we know which types it might be,
since the list of types itself might be polymorphic.
Existential types are ones whose type has been
forgotten by the type system. As a result, we can use
them to allow us to store any type inside of our open sum
container. We will constrain this later on.
Although they’re not necessary, as we’ve seen,
GADTs provide a nice interface for defining and working
with existential types."
-}

import Data.Kind (Type)
import Data.Proxy
import Fcf
import qualified GHC.TypeLits as TL
import GHC.TypeLits hiding (type (+), type (<=), type (>), type (<), type (<=))  
import Unsafe.Coerce
  
-- | OpenSum is a container of f t, where t has kind K
--  f an indexed type, which means it provides a TYPE when given a K
--  It’s a common pattern in type level programming 
-- to label raw data constructors as Unsafe,
-- and write smart constructors that enforce the safety.
-- t must be one of the elements in ts
-- as an example OpenSum ((->) String) '[Int, Bool] is capable of storing 
-- String ->  Int and String -> Bool
-- Int is index number of t in ts
data OpenSum (f::k -> Type)(ts::[k]) where
  UnsafeOpenSum :: Int -> f t -> OpenSum f ts

-- FindIndex (from Fcf) search the type t we are looking for
-- if it finds it return Maybe Nat ; if fails to find it return 'Nothing  
-- >>>:kind! Eval (FindIndex ((<=) 3) '[1, 2, 3, 1, 2, 3])
-- Eval (FindIndex ((<=) 3) '[1, 2, 3, 1, 2, 3]) :: Maybe Nat
-- = 'Just 2

-- >>>:kind! Eval (FindIndex ((>) 2) '[1,2,3,1,2,3])
-- Eval (FindIndex ((>) 2) '[1,2,3,1,2,3]) :: Maybe Nat
-- = 'Just 0
type FindElem (key :: k) (ts::[k]) =
  FromMaybe Stuck =<< FindIndex (TyEq key) ts

-- | the returned index thru FindIndex via Just Nat 
-- we can expose this value by using KnownNat 
--  
type Member t ts = TL.KnownNat (Eval (FindElem t ts))

-- |the type-level nature of FindElem means we
-- pay no runtime cost for the computation.
findElem :: forall t ts. Member t ts => Int
findElem = fromIntegral . natVal $ (Proxy @(Eval (FindElem t ts)))

-- | smart safe constructor for OpenSum
-- inj allows injecting a f t into any OpenSum f ts so long
-- as t is an element somewhere in ts. H
inj :: forall f t ts. Member t ts => f t -> OpenSum f ts
inj = UnsafeOpenSum (findElem @t @ts)

-- | runtime check for if ;
-- the Int type tag inside of OpenSum is the same as
-- the type we’re trying to extract it as
-- if same; we use unsafeCoerce to give non-existential t 
-- if not same; we get Nothing  
prj :: forall f t ts. Member t ts => OpenSum f ts -> Maybe (f t) 
prj (UnsafeOpenSum i f) = 
    if i == findElem @t @ts
      then Just $ unsafeCoerce f
      else Nothing