module RolesExamples where

-- |  the role system ensures coercions are safe
--    type system ensures types are used correctly 
--    kind system ensures types are logical

-- Every type parameter for a given type constructor has a role
-- Roles describe how a type represential equality is related to
-- its parameters coercision safey
{-
  * nominal—the everyday notion of type-equality in
Haskell, corresponding to the a ∼ b constraint. For
example, Int is nominally equal only to itself.
  * representational—as discussed earlier in this
chapter; types a and b are representationally equal if
and only if it’s safe to reinterpret the memory of an
a as a b.
  * phantom—two types are always phantom-ly equal
to one another.
-}

-- in Map k v; Coercible k1 k2 does not imply Coercible (Map k1 v) (Map k2 v)
-- k is at a nominal role; (Map k1 v) (Map k2 v) can be Coercible if k1 ~ k2 

-- newtype Sum a; a is a represential role 
-- so if Coercible  a b implies => Coercible (Sum a) (Sum b)

-- phantom roles are reserved phantom type variables like in Proxy 
-- data Proxy a = Proxy; a is at phantom role
-- Coercible (Proxy a) (Proxy b) is always True;
-- phantom type a does not exist at runtime; it is safe to change when needed

-- roles can be upgrade; upgrading from a weaker role is known as strengthening
-- roles are inferred by the compiler; rules of inferring roles
    -- all type parameters are assumed at role phantom 
    -- type constructor (->) has 2 representational roles; any type
    -- parameterapplied to (->) gets upgrade to representational
    -- Data constructor count as applying (->)
    -- type constructor (~) has 2 nominal roles. any type parameter
    -- applied to (~) upgraded to nominal role; GADTs and type families count
    -- as applying (~)

-- >>>:k Either 
-- Either :: * -> * -> *
-- since Either has (-> constructor) 
-- Either a b should have 2 representational roles

