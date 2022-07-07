{-# LANGUAGE TypeFamilies #-}
-- {-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module TypeFamilies where

-- | Type synonym family ; open 
-- open means more instances can be added
-- closed means you can not add more instances therefore
--  a catch all instance is typically defined for all other types
type family Simplfy t 

type instance Simplfy Integer = Integer
type instance Simplfy Int     = Integer
type instance Simplfy Double  = Integer
type instance Simplfy String  = String
type instance Simplfy Char    = String
type instance Simplfy Bool    = String

class Simplifier t where
    simplify :: t -> Simplfy t

instance Simplifier Integer where
    simplify = id

instance Simplifier Int where
    simplify = fromIntegral
    
-- | use case;
-- >>> simplify (1.2::Double)
-- 1
instance Simplifier Double where
    simplify = round

instance Simplifier String where
    simplify = id

-- | use case;
-- >>> simplify 'c'
-- "c"
instance Simplifier Char where
    simplify =  (:"")

-- | use case;
-- >>> simplify True   
-- "True"
instance Simplifier Bool where
    simplify =  show