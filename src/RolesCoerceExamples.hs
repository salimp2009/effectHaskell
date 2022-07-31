{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- {-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module RolesCoerceExamples where

import Data.Coerce (Coercible(..), coerce)
import Data.Foldable (toList)
import qualified Data.Map as M      -- << qualified because another definition exist in TypelevelFunctions module
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
-- Map dependends on Ord k to determine the where to put the value v
-- therfore its representation in memory depends on Ord k
-- and that is important when it comes to coerce since it relies on that representation 
-- >>>:t M.insert
-- M.insert :: Ord k => k -> a -> Map k a -> Map k a

newtype Reverse a = Reverse 
  { getReverse :: a 
  } deriving (Eq, Show)

-- | in this example Reverse chages the Ord instance paramaters
-- insteads compare a b   it uses compare b a
-- and it is safely Coercible which is not the case for Map
-- since it will change its layout in memory
-- using Map (Reverse k) v will change
-- layout of v value and is not safe to coerce
-- but we can however use v to coerce since the layout does not depend value v
-- Haskell will allow to coerce only it is safe
instance Ord a => Ord (Reverse a) where
  compare (Reverse a) (Reverse b) = compare b a

-- | example for showing coerce safely for Map using Reverse as the value v
-- singleton gives us a Map with the give arguments
-- coerce converts to a similar structure with the same representation
-- in this case it is a List of tuples; first element being key and second as value
-- >>> coerce (M.singleton 'S' True) :: M.Map Char (Reverse Bool)
-- fromList [('S',Reverse {getReverse = True})]

-- | unsafe coerce example
-- since the key type parameter is of type Reverse which changes the layout of Map
-- coerce should not allow this 
-- >>>coerce (M.singleton 'S' True) :: M.Map (Reverse Char) Bool
-- Couldn't match type ‘Char’ with ‘Reverse Char’
--   arising from a use of ‘coerce’


