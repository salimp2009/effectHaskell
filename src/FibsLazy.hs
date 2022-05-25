module FibsLazy where

fib :: (Eq a, Num a, Num p) => a -> p
fib n
    | n == 0 = 0
    | n == 1 = 1
    | otherwise = fib (n-1) + fib (n-2)

-- | (<$>) is similar to fmap f;
-- so it is equal to map fib [0..] and it is lazy 
fibs :: [Integer]
fibs = fib <$> [0..]

-- | use case: smallFibs 10 -> [0,1,1,2,3,5,8]
-- smallFibs 100 -> [0,1,1,2,3,5,8,13,21,34,55,89]
smallFibs :: Integer -> [Integer]
smallFibs n = takeWhile (< n) fibs