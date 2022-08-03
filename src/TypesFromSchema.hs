{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
--{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module TypesFromSchema where
{- 
  a data-structure to store the format schema
  of a printf call. This can be done by building a binary type
  constructor which is polykinded in both of its parameters.
  The goal is to build a type-safe, heterogeneously-kinded
  linked-list.
-}
import Data.Kind(Type)
import Data.Monoid((<>))
import Data.Proxy(Proxy(..))
import GHC.TypeLits

-- | the (:<<) has no special meanning in Haskell
-- >>>:set -XNoStarIsType 
-- >>>:k (:<<)
-- (:<<) :: k1 -> k2 -> Type

-- (:<<) works as cons-cell for our linked-list
-- we can chain them together indefinetly and store at type level
-- >>>:kind! "hello" :<< String :<< "!"
-- "hello" :<< String :<< "!" :: *
-- = "hello" :<< (String :<< "!")
data (a::k1) :<< (b::k2)
infixr 5 :<<

-- | associate type family ; type Printf a
-- every instance of HasPrintf will implement this
class HasPrintf a where
  type Printf a :: Type
  format :: String -> Proxy a -> Printf a

-- | to simplify our format type will be in form
--  a :<< ...:<< "symbol"
-- this allows us to build a structural recursion
-- Structural recursion is technique of producing 
-- something by tearing a recursive structure (like our data (a::k1) :<< (b::k2))
-- apart into smaller pieces; fancy name for "divide and conquer"
-- printf example will require 3 case ;
{- 
  1. HasPrintf (text :: Symbol)
  2. HasPrintf a => HasPrintf ((text :: Symbol) :<< a)
  3. HasPrintf a => HasPrintf ((param :: Type) :<< a)
-}

-- | adding format method to be able to show the terms
-- with Printf method we get the types only
-- with format we get the terms and types
instance KnownSymbol text => HasPrintf (text::Symbol) where
  type Printf text = String
  format s _ = s <> symbolVal (Proxy @text)
-- | The second case corresponds to having additional
-- text we want to inject into our final formatted string. 
-- we don’t have a parameter available to
-- consume, and so here we don’t change the resulting
-- simpler type of Printf. 
instance (HasPrintf a, KnownSymbol text) => HasPrintf ((text :: Symbol) :<< a)  where
  type Printf (text :<< a) = Printf a
  format s _ = format (s <> symbolVal (Proxy @text) ) (Proxy @a)

 {- | Case 3; here we want to add our
  param type as a parameter to the generated function. 
  We can do that by defining Printf as an arrow type that takes
  the desired parameter, and recurses.
 -} 
-- use case ;
-- >>>:set -XNoStarIsType
-- >>>:kind! Printf (Int :<< ":" :<< Bool :<< "!")
-- Printf (Int :<< ":" :<< Bool :<< "!") :: Type
-- = Int -> Bool -> String

-- >>>:set -XNoStarIsType
-- >>>:kind! Printf (Int :<< "double" :<< Double :<< "bool" :<< Bool :<< "!") :: Type
-- Printf (Int :<< "double" :<< Double :<< "bool" :<< Bool :<< "!") :: Type :: Type
-- = Int -> Double -> Bool -> String
instance (HasPrintf a, Show param) => HasPrintf ((param :: Type) :<< a)  where
  type Printf (param :<< a) = param -> Printf a
  format s _ param = format (s <> show param ) (Proxy @a)

-- |use cases for printf;
-- >>>printf (Proxy @"test") 
-- "test"

-- >>>printf (Proxy @(Int :<< " + " :<< Int :<< " = 3")) 1 2
-- "1 + 2 = 3"


-- >>>printf (Proxy @(String :<< "world!")) "hello"
-- "\"hello\"world!"  
printf :: HasPrintf a => Proxy a -> Printf a
printf = format ""


-- | one flaw for String as a type get extra set of quotes ""
-- need to add special case for String using Overlapping and FlexibleINstances
-- FlexibleInstances is required , since the instance head is
-- no longer just a single type constructor and type
-- variables
-- OVERLAPPING is used because we dont want general case#3 with param::Type
-- we want to use this special instance for Strings
-- >>>printf (Proxy @(String :<< " world!")) "hello"
-- "hello world!"
instance {-# OVERLAPPING #-} HasPrintf a => HasPrintf (String :<< a) where
  type Printf (String :<< a) = String -> Printf a
  format s _ param  = format (s <> param) (Proxy @a)

  -- | Note on OVERLAPPING for type families ;
  {- 
    "..in general, they’re not
    allowed. The reason we can overlap param :<< a and
    String :<< a is that they actually agree on the type family
    instance. When param ∼ String, both instances give
    Printf (param :<< a) to be String -> Printf a
    .."

    "..Using type-level programming, we were able to convert
    such a thing into a function with the correct type, that
    implements nontrivial logic.
    This technique is widely-applicable. For example, the
    popular servant[11] library uses a similar type-level
    schema to describe web APIs, and will generate typesafe
    servers, clients and interop specs for them"
  -}
