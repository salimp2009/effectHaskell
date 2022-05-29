module FunctsWithFolds where

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


myFilter'' :: Foldable t => (a -> Bool) -> t a -> [a]
myFilter'' p xs = foldl filterHelper id xs mempty
    where
        filterHelper r x   = r.f x
            where
                f x r
                    | p x = x : r
                    | otherwise = r


myFilter' :: (a -> Bool) -> [a] -> [a]
myFilter' _ [] = []
myFilter' pred (x:xs)
            | pred x  =  x : myFilter' pred xs
            | otherwise = myFilter' pred xs