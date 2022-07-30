{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module RolesCoerceExamples where

import Data.Coerce (Coercible(..), coerce)
import Data.Foldable (toList)
import qualified Data.Map as M
import Data.Monoid ( Sum(..), Product(..) )

-- | fmap a list of integers and constructing list Sum types
-- is slow operation
slowSum :: [Int] -> Int
slowSum = getSum . mconcat . fmap Sum

-- | this is O(0) time using coerce
-- converting [Int] -> [Sum] since their representation in
-- memory is same ; this is also used in newtype types
-- if the functor (e.g []) is polymorphic then coerce
-- will not work since it will not know what type a  will be
-- but there is also unsafecoerce 
-- for cases you guarantee the type of a has same representation
-- with the converted type b
fastSum :: [Int] -> Int
fastSum = getSum . mconcat . coerce

-- | since Coercible uses representational equality
-- it follows laws of equality;
{- 
  * Reflexivity—Coercible a a is true for any type a
  * Symmetry—Coercible a b implies Coercible b a
  * Transitivity—given Coercible a b and Coercible b c
    we have Coercible a c
-}
-- | both Sum and Product are newtypes therefore used
-- the representation of the type they wrap
-- >>>coerce (1867 :: Sum Int) :: Product Int
-- Product {getProduct = 1867}

-- | examples to show if it is safe to coerce 
-- representationally equal types
-- example uses a container Map (from Data.Map)  
-- with a look up Map k v 
-- a balanced tree via Ord k instance
-- >>>:t M.insert
-- M.insert :: Ord k => k -> a -> Map k a -> Map k a

