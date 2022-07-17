--{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module TypeScopingApplications where

-- | example to show that the type b at top level
-- and the type in the apply function top annotation
-- are considered different according to GHC (Hindley Milner type system)
-- can be fixed by ScopedTypedVariable and using forall a b. at top annotation
-- brokennew :: (a -> b) -> a ->b
-- brokennew f a = apply
--     where
--         apply:: b
--         apply = f a

workingNew :: forall a b. (a -> b) -> a -> b
workingNew f a = apply
    where 
        apply :: b
        apply = f a

-- | basic example of type application
-- read has a type ; 
-- read :: forall a. Read a => String -> a 
-- we specify type a by using type application
-- this also required ScopedTypeVariables and TypeApplications extension
-- Type application can be in a function body only
-- | use case;
-- >>> readInt "1"      
-- 1
readInt :: String -> Integer
readInt = read @Integer

-- | use case;
-- >>> readFloat "1"
-- 1.0
readFloat :: String -> Float
readFloat = read @Float

-- | although we can use type application on read
-- we cannot use it in branches directly
-- but we can use it by using the function in another function
-- see below; 
-- >>> showLeftRight @Float @Int "3.456454"
-- Left 3.456454

-- >>> showLeftRight @Float @Int "321" 
-- Right 321

-- >>>showLeftRight @Float "3.456454"
-- Left 3.456454

-- >>> showLeftRight @_ @Int "321" 
-- Right 321
showLeftRight :: forall a b. (Read a, Read b) => String -> Either a b
showLeftRight s
    | length s > 5 = Left  (read s)
    | otherwise    = Right (read s)

-- | use case;
--  >>> useshowLeftRight "1234534"   
-- WAS Left 1234534.0
-- NOW Left 1234534.0
-- >>> useshowLeftRight "123"
-- Right 123
useshowLeftRight :: String -> Either Float Int
useshowLeftRight = showLeftRight @Float @Int   

