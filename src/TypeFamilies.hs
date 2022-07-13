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


-- | Closed type family example
type family Widen a where
    Widen Bool  = Int
    Widen Int   = Integer
    Widen Char  = String
    Widen t     = String  -- << this is the catch all other case      

-- | helper type class to use the type family
-- at     
class Widener a where
    widen :: a -> Widen a
    
instance Widener Bool where
    widen True  = 1
    widen False = 0

instance Widener Int where
    widen = fromIntegral   
    
instance Widener Char where
    widen c = [c]