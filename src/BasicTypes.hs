{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- ^ These are used for the example working / working Scoped Types Variable

module BasicTypes where

-- | basic example to show how type annotations can be added
-- to let and where bindings
calculateTotalCost :: Int -> Int
calculateTotalCost basePrice =
    let
        priceWithServiceFee :: Int
        priceWithServiceFee = basePrice  + 20
        customaryTip = 6 :: Int
    in priceWithServiceFee + customaryTip

newaddOne :: Int -> Int
newaddOne = (+ 1)

newaddThreeNums :: Int -> (Int -> (Int -> Int))
newaddThreeNums x y z =
    let
        f:: Int -> (Int -> (Int -> Int))
        f a =
            let
                g :: Int -> (Int -> Int)
                g b =
                  let
                    h :: Int -> Int
                    h c = a + b + c
                  in h
            in g
    in f x y z

-- | example from book Thinking types
-- only works when forall is applied and also have to use 
-- -XScopedTypeVariables extension
-- if no type annotation is supplied for apply it works; compiler works it
working :: forall p t. (p -> t) -> p -> t
working f a = apply
    where
        apply :: t
        apply = f a

-- broken :: p1 -> p2 -> t
-- broken f a = apply
--     where
--         apply :: t
--         apply = f a 

working''::(a -> b) -> a ->b
working'' f a = apply
    where apply = f a

-- | this work all type variable are explicitly Int
-- if this is generic a ->b ->b -> b then
-- we had to use forall a b . and for the scope type variable
-- otherwise 
-- worksToo :: (Int -> Int) -> Int -> Int
worksToo :: forall a b. (a -> b) -> a -> b
worksToo f a =
     let
        -- apply:: (Int -> Int) -> Int -> Int
        apply:: (a -> b) -> a -> b
        apply f' =
             let
                 --applymore :: Int -> Int
                 applymore ::  a -> b
                 applymore a' = f' a'
             in applymore
    in apply f a

 -- | use case;  incrementAndShow 2 show -> "3" 
incrementAndShow :: Int -> (Int -> String) -> String
incrementAndShow n formatter = formatter (n+1)

-- | use case : 
-- incrementAndShow' 2 (&) -> "3"
-- ^ need to import Data.Function ((&)) to be able to use it
-- | use case ; 
-- incrementAndShow' 2 incrementAndShow -> "4"
incrementAndShow' :: Int -> (Int -> (Int ->String)->String) ->String
incrementAndShow' n f = f  (n+1) show

pointful :: [Int] -> Int -> Int
pointful xs n = sum xs * n

etaReducedSum :: [Int] -> (Int -> Int)
etaReducedSum xs = (*) (sum xs)

pointFreeSum :: [Int] -> Int -> Int
pointFreeSum = (*) . sum

apply' :: (a -> b) -> a -> b
apply' f val = f val

incrementInt :: Int -> Int
incrementInt n = n + 1

incrementInted :: Int
incrementInted = apply' incrementInt 1

    



