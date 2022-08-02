{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
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

instance HasPrintf (text::Symbol) where
  type Printf text = String

-- | The second case corresponds to having additional
-- text we want to inject into our final formatted string. 
-- we don’t have a parameter available to
-- consume, and so here we don’t change the resulting
-- simpler type of Printf. 
instance HasPrintf a => HasPrintf ((text :: Symbol) :<< a)  where
  type Printf (text :<< a) = Printf a

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
instance HasPrintf a => HasPrintf ((param :: Type) :<< a)  where
  type Printf (param :<< a) = param -> Printf a
