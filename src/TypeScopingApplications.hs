--{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilies #-}
module TypeScopingApplications where
import Data.Typeable (Typeable, typeRep)
import Data.Data (Proxy(..))

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
-- Left 1234534.0

-- >>> useshowLeftRight "123"
-- Right 123
useshowLeftRight :: String -> Either Float Int
useshowLeftRight = showLeftRight @Float @Int

-- | use of forall; universal quantification referred as explicit forall
-- use cases; different type can be used for val and the type will be inferred by GHC
-- >>> adheresToReadShowContract 23.4656
adheresToReadShowContract :: forall a. (Read a, Show a) => a -> Bool
adheresToReadShowContract val =
    let a = show . read @a . show $ val
        b = show val
    in a == b


-- | there are two types GHC uses; inferred and specified
-- when we have an input value and there is no type annotation
-- GHC infers those
-- there is also specified types which we specify via TypeApplication
-- if a type is inferred type application is not allowed
convertViaInt :: forall a b. (Integral a, Num b) => a -> b
convertViaInt input = fromIntegral $ fromIntegral @_ @Int input

-- | this version does not work since we have no top level annotation
-- GHC infers the type of a and b
-- when we try to specify then it 

-- >>> converttoInt @Int 5
-- Cannot apply expression of type ‘w0 -> b0’
-- to a visible type argument ‘Int’

-- >>>:set -fprint-explicit-foralls
-- >>>:t converttoInt
-- converttoInt :: forall {w} {b}. (Integral w, Num b) => w -> b

-- | {w} {b} ; the brackets around w and b indicates those types are inferred
converttoInt a = fromIntegral $ fromIntegral @_ @Int a

-- | we can specify explicitly which types will be inferred and specified
-- use cases;
-- >>>convertViaInt2 @Double 5
-- 5.0

-- >>> convertViaInt2 @Int 5
-- 5

-- >>> convertViaInt2 @Int @Double 5
-- Cannot apply expression of type ‘a0 -> Int’
-- to a visible type argument ‘Double’
-- ^ we can not specify type for a type we explictly set to be inferred; {a}
convertViaInt2 :: forall {a} b. (Integral a, Num b) => a -> b
convertViaInt2 input = fromIntegral $ fromIntegral @_ @Int input

-- | type parameter a is not used on the right side of (=>) the type annotation
-- therefore we need to use {-# LANGUAGE AllowAmbiguousTypes #-}
-- we use type application for Proxy which is used by typeRep; only to determine the type
-- we could also write Proxy :: Proxy Int
-- use case;
-- >>> typename @Double
-- "Double"

-- >>> typename @Int
-- "Int"

-- >>> typename @(Maybe [Int])  
-- "Maybe [Int]"
typename :: forall a. Typeable a => String
typename = show . typeRep  $ Proxy @a

-- | example for Ambigous types
type family AlwaysUnit a where
    AlwaysUnit a = ()

-- | Which one is ambigous ?    
undefined1 :: AlwaysUnit a -> a
undefined1 = undefined

undefined2 :: b -> AlwaysUnit a -> a
undefined2 = undefined

-- | Ambigous because type parameter a can be 
-- any given type and Show can not determine 
-- which type instance to apply
-- Always unit is non-injective because given AlwaysUnit a
-- we can not determine the type of a
undefined3 :: Show a => AlwaysUnit a -> String
undefined3 = undefined

-- | Ambigous because Show 
undefined4 :: forall a. Show a => AlwaysUnit a -> String
undefined4 = show