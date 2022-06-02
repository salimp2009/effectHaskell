module UsingUnDefined where


-- | Example use case how to use undefined during 
-- intermediate steps of implementation to test the types
-- it compiles but needs to implement the undefined functions
allNums :: [[a]]
allNums = undefined

sumBiggest :: [[Int]] -> String
sumBiggest allNums = 
    let 
        getBiggests :: [Int] -> [Int]
        getBiggests = undefined

        getSmallests :: [Int] -> [Int]
        getSmallests = undefined
    in undefined

allBiggests :: [[Int]] 
allBiggests = map getBiggests allNums
    where
        getBiggests = maximum

allSmallests :: [[Int]] 
allSmallests = map getSmallests allNums
    where
        getSmallests = minimum

sizePairs:: [([Int], [Int])]
sizePairs = zip allBiggests allSmallests

differences :: ([Int], [Int]) -> Int
differences = undefined

differences' ::[String]
differences' = map (show.differences) sizePairs


