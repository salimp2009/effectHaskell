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

-- | the argument r in => f r x is the accumulate
-- the initial value for is id so => id. f' x1
-- so we have id.f' x1 . f' x2 . ... .f' xn $ mempty 
--  r' has type [b] -> [(a, b)] ; it consumes ys
-- so accumulate r becomes (x1, y1) : r y1s
-- it continues either of the list consumed
-- => (x1, y1) : (x2, y2) : .... : (xn, yn) : mempty
myzip3 :: Foldable t => t a -> [b] -> [(a, b)]
myzip3 xs ys = foldl f id xs mempty ys
    where
        f r x  = r . f' x                    
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