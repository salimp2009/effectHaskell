{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
--{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Defunctionalization where
{- 
  Defunctionalization—the process of replacing
  an instantiation of a polymorphic function with a
  specialized label instead.
  
  The main goal is to type families as a first class
  as they are unable to be partially applied, reuse and abstraction
  limitations.
  
  By using defunctionalization tech we will work around those
  type family limitations
-}  

import Prelude hiding (fst)

-- | instead of using fst function
-- fst :: (a, b) -> a
-- that gives us the first element of tuple
-- we define a type constructor ; given two types
-- it constructs a tuple 
newtype Fst a b = Fst (a, b)

-- | Eval type class will be used to implement
-- the function we need over our Fst type
{-
  The syntax | l -> t at 1 is known as a functional
  dependency, and states that the type t is fully determined
  by the type l. 
  l is the type of our defunctionalized
  label, and t is the return type of the evaluation.
-}
class Eval l t | l -> t  where
  evalt :: l -> t

-- | use case;
-- >>>evalt (Fst ("salitos", 5))  
-- "salitos"
instance Eval (Fst a b) a where 
  evalt (Fst (a, b)) = a

newtype LstToMaybe a  = LstToMaybe [a]

-- | use case;
-- >>>evalt (LstToMaybe [1..5])
-- Just 1
instance Eval (LstToMaybe a) (Maybe a) where
  evalt (LstToMaybe [])      = Nothing
  evalt (LstToMaybe (x : _)) = Just x

-- | example for defunctionalizing a higher-functions
-- wrapped in newtype 
-- evaluation of MapList will depend on Eval dfb instance
-- the reason is to propogate evaluation to the dfb which is 
-- also defunctionalized
-- use  case;
-- >>>evalt (MapList Fst [(1,2), (2,4), (3,5)])
-- [1,2,3]

-- >>>evalt (MapList LstToMaybe  [[1,2], [2,4], [3,5]])
-- [Just 1,Just 2,Just 3] 
data MapList dfb a = MapList (a -> dfb) [a] 

instance Eval dfb dft => Eval (MapList dfb a) [dft] where
  evalt (MapList _ []) = []
  evalt (MapList f (x: xs )) 
    = evalt (f x) : evalt (MapList f xs)