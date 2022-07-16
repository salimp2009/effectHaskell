{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}
module TypeLevelFunctions where

-- | example to create a type level function
-- using closed type family
-- ::Bool at the the end is the return type of the function
-- similar term level functions     Bool -> Bool -> Bool
-- type level function version is   (x::BOOL) (y::BOOL) :: BOOL
-- all type level functions must be total no partial function allowed 
-- >>>:k (Or 'True 'False)    
-- (Or 'True 'False) :: Bool
-- = 'True
type family Or (x::Bool) (y::Bool)::Bool where
    Or 'True  _ = 'True
    Or 'False y = y

-- | Partial application of type level functions not allowed    
-- >>>:k (Map (Or 'True) '[ 'True, 'False, 'True])    
-- The type family ‘Or’ should have 2 arguments, but has been given 1
type family Map (x :: a -> a) (i::[a]) :: [b] where
    Map f '[] = '[]
    Map f (x ': xs) = f x ': Map f xs

-- >>>:kind! Foo
-- Foo :: Bool -> Bool -> Bool
-- = Foo
type family Foo (x::Bool) (y::Bool)::Bool where

-- >>>:kind! Bar
-- Bar :: k -> k1 -> Bool -> Bool -> Bool
-- = Bar
type family Bar x y ::Bool -> Bool -> Bool where    
