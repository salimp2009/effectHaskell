module ProgramEval where

import FoldFunctions (myFoldr)

numbersStartingAt :: Num t => t -> [t]
numbersStartingAt n =
            n: numbersStartingAt (n+1)

radsToDegrees::Float -> Int
radsToDegrees radians =
    let degrees = cycle [0..359]
        converted = truncate $ (radians * 360) / (2 * pi)
    in degrees !! converted

-- | Manual/Naive way of creating lazy cycle
-- usage: epicCycle [0..4] $  !! 10  -> 0 ; 
-- e.g: similar to modulo operation 10 `mod` 5
epicCycle :: [a] -> [a]
epicCycle inputList = 
            cycleHelper inputList 
            where
                cycleHelper [] = epicCycle inputList
                cycleHelper (x:xs) = x : cycleHelper xs

-- | Similar to above creating cycle lazily
moreEpicCycle :: Semigroup t => t -> t
moreEpicCycle inputList =
     inputList <> moreEpicCycle inputList

findFirst :: (a -> Bool) -> [a] -> [a]
findFirst predicate =
    myFoldr findHelper []
    where
        findHelper listElement maybeFound
            | predicate listElement = [listElement]
            | otherwise = maybeFound


findFirst' :: (a -> Bool) -> [a] -> [a]
findFirst' predicate lst
        | null lst = []
        | predicate (head lst) = [head lst]
        | otherwise = findFirst' predicate (tail lst)