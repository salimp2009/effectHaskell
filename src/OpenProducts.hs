{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module OpenProducts where

import Data.Kind (Constraint, Type)
import Data.Proxy (Proxy (..))
import qualified Data.Vector as V
import Fcf
import GHC.OverloadedLabels (IsLabel (..))
import GHC.TypeLits 
import Unsafe.Coerce (unsafeCoerce)

 -- | a container Any that will
-- existentialize away its k index.
data Anyc (f::k -> Type) where
  Anyc :: f t -> Anyc f

{-
  "This implementation of OpenProduct will to optimize
  for O(1) reads, and O(n) writes, although other trade-offs
  are possible. We thus define OpenProduct as a Data.Vector
  of Anys."
-}  

-- | ts is now keeps track of which type store in Vector Anyc
-- and it also associates them with names as Symbols so user can provide
-- names for the contents of the product
data OpenProduct (f::k -> Type) (ts :: [(Symbol, k)]) where
  OpenProduct :: V.Vector (Anyc f) -> OpenProduct f ts

nil :: OpenProduct f '[]  
nil = OpenProduct V.empty

{- |
  "Because all data inside an OpenProduct will be labeled by
  a SYMBOL, we need a way for users to talk about SYMBOLs at
  the term-level."
-}
-- | this will allow us to use Key @"myData"  
-- (needs TypeApplications extension)
data Key (key :: Symbol) = Key

-- | cons :: a -> Vector a -> Vector a (/O(n)/ Prepend an element.)
-- use cases;
-- >>>result = insert ( Key @"salitoskey ") ( Just "didem") nil
-- >>>:t result
-- >>>:t insert (Key @"semoskey") (Just "demir") result
-- >>>:t insert (Key @"semsoskey") (Just True) result
-- result :: OpenProduct Maybe '[ '("salitoskey ", String)]
-- insert (Key @"semoskey") (Just "demir") result
--   :: OpenProduct
--        Maybe '[ '("semoskey", String), '("salitoskey ", String)]
-- insert (Key @"semsoskey") (Just True) result
--   :: OpenProduct
--        Maybe '[ '("semsoskey", Bool), '("salitoskey ", String)]

insert :: Key key -> f t -> OpenProduct f ts -> OpenProduct f ('(key, t) ': ts)
insert _ ft (OpenProduct v) =  OpenProduct $ V.cons (Anyc ft) v

-- | prevent multiple same keys since it migh be confusing
{-
  data Null :: [a] -> Exp Bool
  type instance Eval (Null '[]) = 'True
  type instance Eval (Null (a ': as)) = 'False
-}
{-
  "UniqueKey is the type-level equivalent of null . filter
  (== key) . fst. If the key doesn’t exist in ts, UniqueKey
  returns 'True. "
-}
type UniqueKey (key::k) (ts::[(k, t)]) = 
    Null =<< Filter (TyEq key <=< Fst) ts
-- >>>result = insert2 (Key @"key") (Just True) nil
-- >>>:t result
-- result :: OpenProduct Maybe '[ '("key", Bool)]
-- >>>:t insert2(Key @"key") (Just False) result 
-- Couldn't match type ‘'False’ with ‘'True’
--   arising from a use of ‘insert2’
-- >>>:t insert (Key @"key") (Just False) result 
-- insert (Key @"key") (Just False) result :: OpenProduct Maybe '[ '("key", Bool), '("key", Bool)]
insert2 :: Eval (UniqueKey key ts) ~ 'True 
        => Key key -> f t -> OpenProduct f ts -> OpenProduct f ('(key, t) ': ts) 
insert2 _ ft (OpenProduct v) =  OpenProduct $ V.cons (Anyc ft) v   

-- | to get the data out of OpenProduct we need a getter
--  First we will do a lookup at type level in list of types (ts:: [(Symbol, k)])
--  it will return us the index of the Vector, where the corresponding type is stored
-- will return us the corresponding Type
type FindElemP (key :: Symbol) (ts :: [(Symbol, k)]) =
  Eval (FromMaybe Stuck =<< FindIndex (TyEq key <=< Fst) ts)

-- | bringing the type level index info into term level  
findElemP :: forall key ts. KnownNat (FindElemP key ts) => Int
findElemP = fromIntegral . natVal $ (Proxy @(FindElemP key ts))

-- | findElemp gives us index number of a key we are looking
-- Lookup will give the type we should be getting out from the Vector
-- so basically the second value of the tuple
-- Lookup :: forall k b. k -> [(k, b)] -> Exp (Maybe b)
type LookupType (key::k)(ts::[(k, t)]) =
  FromMaybe Stuck =<< Lookup key ts

{-
  "Since we’ve been careful in
  maintaining our invariant that the types wrapped in our
  Vector correspond exactly with those in ts, we know it’s
  safe to unsafeCoerce"
-}
get :: forall key ts f. KnownNat (FindElemP key ts)  
    => Key key -> OpenProduct f ts -> f (Eval (LookupType key ts))
get _ (OpenProduct v) = unAnyc $ V.unsafeIndex v $ findElemP @key @ts
  where 
    unAnyc (Anyc a) = unsafeCoerce a

-- | to update an existing key, use SetIndex which needs the key and 
-- new pair with key and type (OpenProduct allows to change types)    
-- data SetIndex :: Nat -> a -> [a] -> Exp [a]
type UpdateElem (key::Symbol) (t::k) (ts::[(Symbol, k)]) =
    SetIndex (FindElemP key ts) '(key, t) ts 
    
update :: forall key ts t f. KnownNat (FindElemP key ts)    
       => Key key -> f t -> OpenProduct f ts -> OpenProduct f (Eval(UpdateElem key t ts))
update _ ft  (OpenProduct v) = OpenProduct $ v V.// [(findElemP @key @ts, Anyc ft)] 

--  type DeleteElem (key::Symbol) (ts::[(Symbol, k)]) =
-- --      Filter ((Fcf.<=) (FindElemP key ts) (Fst =< ) )ts

type DeleteElem key = Filter (Not <=< TyEq key <=< Fst) 

delete :: forall key ts f. KnownNat (FindElemP key ts) 
       => Key key -> OpenProduct f ts -> OpenProduct f (Eval(DeleteElem key ts))
delete _ (OpenProduct v) =
          let (v1, v2) = V.splitAt (findElemP @key @ts) v
          in OpenProduct $ v1 V.++ V.tail v2

