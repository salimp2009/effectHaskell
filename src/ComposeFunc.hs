module ComposeFunc where


myadd :: Num a => a -> a -> a
myadd a b = a + b

myMultip :: Num a => a -> a -> a
myMultip a b = a * b

myAddMultip :: Num b => b -> b -> b -> b
myAddMultip b = myadd  . myMultip b

addOne' :: Num a => a -> a
addOne' num = num + 1

timesTwo :: Num a => a -> a
timesTwo num = num * 2

squared :: Num a => a -> a
squared num = num * num

minusFive :: Num a => a -> a
minusFive num = num - 5

findResult :: Num a => a -> a
findResult = minusFive.squared.timesTwo.addOne'

timesEight :: Integer -> Integer
timesEight = timesTwo . timesTwo . timesTwo

maybeTail :: [a] -> Maybe [a]
maybeTail [] = Nothing
maybeTail (a:as) = Just as

liftmyMaybe :: (Functor f1, Functor f2) => (a -> b) -> f1 (f2 a) -> f1 (f2 b)
liftmyMaybe = (<$>) . (<$>)

liftmyfunctors :: (Functor f1, Functor f2) => (a -> b) -> f1 (f2 a) -> f1 (f2 b)
liftmyfunctors = fmap. fmap

-- | I dont think this is correct and the end result might be an empty list
isperfectPower :: [Int] -> Int -> Int -> [(Int, Bool)]
isperfectPower [] _ _= []
isperfectPower (a:as) k upperBound
                | a*a == k =  (a, True) : isperfectPower [a] (k+1) upperBound
                | k == upperBound = isperfectPower [] upperBound upperBound
                | otherwise = rs
                  where rs = isperfectPower as (k + 1) upperBound



