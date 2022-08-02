{-# LANGUAGE TypeFamilies #-}
--{-# LANGUAGE TypeApplications #-}
--{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RoleAnnotations #-}
module RolesExamples where
import Data.Coerce (coerce)
import Data.Kind(Type(..))
import Unsafe.Coerce ( unsafeCoerce )

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

-- | close type family
type family IntToBool a where
  IntToBool Int = Bool
  IntToBool a   = a

-- | it is possible to strengthen an inferred role to a less
-- permissive one by using extension RoleAnnotations
-- example Binary Search Trees;
-- BSTs have memory dependencies on their Ord instance
data BST v  
       = Empty
       | Branch (BST v) v (BST v)

-- | the type v is inferred by default as representational
-- by applying a less weeker role by type role 
-- the syntax is ; 
-- type role TypeConst role1 role2 ...
-- define a role for each type parameter in the same order defined
-- type role cant be used for weaking ; 
-- eg from representational to phantom wont compile
type role BST nominal      

newtype Age = Age Int
  deriving Show

toAges :: [Int] -> [Age]  
toAges = coerce

-- | example to show type families 
-- can not be coerced ; 
-- HLS error msg ; 
-- • Couldn't match representation of 
  -- type ‘Int’ with that of ‘Bool’
-- toIntToBool :: Int -> IntToBool Int
-- toIntToBool = coerce

data Student ageType = Student String ageType

check :: Student Int -> Student Age
check = coerce

data Student1 ageType = Student1 String (Maybe ageType)

check2 :: Student1 Int -> Student1 Age
check2 = coerce

data Student2 m ageType = Student2 String (m ageType)

-- | this wont compile
-- because ageType has nominal role
-- HLS show the error; 
--  "Couldn't match type ‘Int’ with ‘Age’
--  arising from a use of ‘coerce’ "
-- check3 :: Student2 Maybe Int -> Student2 Maybe Age
-- check3 = coerce 

-- | open type family
type family Id t 
type instance Id t = t

data Student3 ageType = Student3 String (Id ageType)
  deriving Show

-- | this wont compile
-- because type families are given (~) therefore has nominal roles on types
-- so ageType for type family has a nominal role
-- therefore it wont work for Int -> Age;
-- only works for Int -> Int
-- or by using unsafecoerce if we are sure they have same representation in memory
-- check3:: Student3 Int -> Student3 Age
-- check3 = coerce

-- | use case ;
-- >>>check3' (Student3 "Salitos" (42::Id Int))
-- Student3 "Salitos" (Age 42)

-- >>>check3' (Student3 "Salitos" (42::Int))
-- Student3 "Salitos" (Age 42)

-- >>>check3' (Student3 "Salitos" 42)
-- Student3 "Salitos" (Age 42)
check3' :: Student3 Int -> Student3 Age
check3' = unsafeCoerce
