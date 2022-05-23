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

-- | usage: e.g: applytoThruple' (1,2,3) -> [1,2,3]
applytoThruple' :: (b, b, b) -> [b]
applytoThruple' as = map ($ as) [myFst, mySnd, myThrd]

-- | _tail is used to communicate _tail will not be used
printHead ::  Show a =>  [a] -> String
printHead [] = "empty!"
printHead lst@(hd: _tail) = 
            "the head of " <> show lst <> " is " <> show hd
