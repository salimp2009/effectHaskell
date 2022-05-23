module PatternMatch where

myFst :: (a, b, c) -> a
myFst (x, _ , _) = x

mySnd :: (a, b, c) -> b
mySnd (_, x, _) = x

myThrd :: (a, b, c) -> c
myThrd (_, _, x) = x  

-- | usage: e.g: applytoThruple [(1,2,3)] -> [1,2,3]
applytoThruple :: [(b, b, b)] -> [b]
applytoThruple = (<*>) [myFst, mySnd, myThrd] 