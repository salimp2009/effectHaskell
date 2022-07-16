{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoStarIsType #-}

module Type_Operators() where

-- | (+) operator on type level indicates it is a sum type
data a + b = Inl a | Inr b
    deriving Show

-- | this extension is needed it gives error{-# LANGUAGE NoStarIsType #-}
data a * b = a :*: b
    deriving Show

infixl 6 +
infixl 7 *

first :: a * b -> a
first (a :*: _) = a

-- | use case; second (5 :*: 4) -> 4 
-- does not work on regular pairs (,)
second :: a * b -> b
second (_ :*: b) = b 

-- | this is similar to Either Int (Bool, Bool)
val1 :: Int + Bool * Bool
val1 = Inl 0

-- | this is similar to Either Int (Bool, Bool)
val2 :: Int + Bool * Bool
val2 = Inr(True :*: False)

-- | due to left assoc. similar to 
-- Point a = (a + (a*a)) + (a*a*a)
type Point a = a + a *a +  a*a*a

zero2D :: Point Int
zero2D = Inl (Inr (0 :*: 0))