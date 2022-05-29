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

-- | just to show steps but inefficient; dont use
-- if used ; e.g:  take 10 $ fibsLazy 0 1 -> [0,1,1,2,3,5,8,13,21,34]
fibsLazy :: Num t => t -> t -> [t]
fibsLazy firstFib secondFib = 
    let nextFib = firstFib + secondFib
    in firstFib : fibsLazy secondFib nextFib 

-- | just to show steps but inefficient; dont use
-- if used; same above
fibsLazy' :: Num a => a -> a -> [a]
fibsLazy' first' second' = first' : fibsLazy' second' (first' + second')


-- | more efficient lazy stream; 
-- usage : take 10 fibsRev -> [0,1,1,2,3,5,8,13,21,34]
fibsRev :: [Integer]
fibsRev = 0 : 1 : helper fibsRev (tail fibsRev)
    where
        helper [] _ = []
        helper as [] = as 
        helper (a:as) (b:bs) =
            a + b : helper as bs

-- ^ fibs = 0 : 1 : helper (0 : 1 : <thunk>) (1 : <thunk>)
-- where
-- helper (0 : 1 : <thunk>) (1 : <thunk>) =
--    0 + 1 : helper (1 : <thunk>) <thunk>

myzipWith :: (t1 -> t2 -> a) -> [t1] -> [t2] -> [a]
myzipWith f = go
    where 
        go [] _ = []
        go _ [] = []
        go (x:xs) (y: ys) = f x y : go xs ys

myzip2 :: Foldable t => t a -> [b] -> [(a, b)]
myzip2 xs ys = foldr f mempty xs ys
    where 
--      f :: a -> ([b] -> [(a, b)]) -> [b] -> [(a, b)]
        f x r [] = []
        f x r (y:ys) = (x,y) : r ys

-- | the function r is just to consume ys which is not used by foldl
-- but it is an argument to function f as id
-- it is also an argument for f' to consume ys
-- need to figure how this works :)      
myzip3 :: Foldable t => t a -> [b] -> [(a, b)]
myzip3 xs ys = foldl f id xs mempty ys
    where 
        f r x a = r (f' x a)                    -- ^ a refers to empty and r refers to id
            where 
                f' _ _ [] = []
                f' x r (y:ys) = (x, y) : r ys


myzip4 :: Foldable t => t a -> [b] -> [(a, b)]
myzip4 xs ys = foldl (\r x a -> r(f x a)) id xs mempty ys
    where
        f _ _ [] = []
        f x r' (y:ys) = (x , y) : r' ys

myFilter :: Foldable t => (a -> Bool) -> t a -> [a]
myFilter p = foldr filterHelper []
        where 
            filterHelper x ys 
                        | p x = x : ys
                        | otherwise = ys


myFilter' :: (a -> Bool) -> [a] -> [a]
myFilter' _ [] = []
myFilter' pred (x:xs) 
            | pred x  =  x : myFilter' pred xs
            | otherwise = myFilter' pred xs



