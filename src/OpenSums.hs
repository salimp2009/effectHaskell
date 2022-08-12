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
import qualified Fcf
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
-- >>>:kind! Fcf.Eval (Fcf.FindIndex ((Fcf.<=) 3) '[1, 2, 3, 1, 2, 3])
-- Fcf.Eval (Fcf.FindIndex ((Fcf.<=) 3) '[1, 2, 3, 1, 2, 3]) :: Maybe
--                                                                Nat
-- = 'Just 2

-- >>>:kind! Fcf.Eval (Fcf.FindIndex ((Fcf.>) 2) '[1,2,3,1,2,3])
-- Fcf.Eval (Fcf.FindIndex ((Fcf.>) 2) '[1,2,3,1,2,3]) :: Maybe Nat
-- = 'Just 0

-- | FindElem works by looking through ts and comparing
-- the first element of each tuple with key
type FindElem (key :: k) (ts::[k]) =
  Fcf.FromMaybe Fcf.Stuck Fcf.=<< Fcf.FindIndex (Fcf.TyEq key) ts

-- | the returned index thru FindIndex via Just Nat 
-- we can expose this value by using KnownNat 
type Member t ts = TL.KnownNat (Fcf.Eval (FindElem t ts))

-- |the type-level nature of FindElem means we
-- pay no runtime cost for the computation.
findElem :: forall t ts. Member t ts => Int
findElem = fromIntegral . natVal $ (Proxy @(Fcf.Eval (FindElem t ts)))

-- | smart safe constructor for OpenSum
-- inj allows injecting a f t into any OpenSum f ts so long
-- as t is an element somewhere in ts. H
inj :: forall f t ts. Member t ts => f t -> OpenSum f ts
inj = UnsafeOpenSum (findElem @t @ts)

-- | runtime check for;
-- if the Int type tag inside of OpenSum is the same as
-- the type we’re trying to extract it as
-- if same; we use unsafeCoerce to give non-existential t 
-- if not same; we get Nothing  
prj :: forall f t ts. Member t ts => OpenSum f ts -> Maybe (f t) 
prj (UnsafeOpenSum i f) = 
    if i == findElem @t @ts
      then Just $ unsafeCoerce f
      else Nothing

-- | decompose gives us information about the types inside OpenSum      
-- if we use zero as the tag we will get the first type via Left of Either
-- we will also get the rest of the types as OpenSum with the rest of types
-- us
decompose :: OpenSum f (t ': ts) -> Either (f t) (OpenSum f ts)     
decompose (UnsafeOpenSum 0 t) = Left $ unsafeCoerce t
decompose (UnsafeOpenSum n t) = Right $ UnsafeOpenSum (n-1) t

{- | weaken;
  In practice, it is also useful to be able to widen the
  possibilities of an open sum. A new function, weaken,
  tacks a x type in front of the list of possibilities
-}
weaken :: OpenSum f ts -> OpenSum f (t ': ts)
weaken (UnsafeOpenSum n t) = UnsafeOpenSum (n+1) t

{- |
  If we want to perform the same logic regardless of
  what’s inside an OpenSum, prj and decompose both feel
  inelegant. We introduce match eliminator which
  consumes an OpenSum in O(1) time.
-}
-- By using a rank-n type(forall t. ) , match is given a function
-- that can provide a b regardless of what’s inside the sum
match :: forall f ts b. (forall t. f t -> b) -> OpenSum f ts -> b
match fn (UnsafeOpenSum _ t) = fn t

