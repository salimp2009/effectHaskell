module Lib where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

addOne :: Num a => a -> a
addOne n = n + 1

addOneShow :: Integer -> String
addOneShow = show <$> addOne

addOneShowReverse :: Integer -> [Char]
addOneShowReverse = reverse <$> addOneShow

withComposition :: Integer -> [Char]
withComposition = reverse.show.(+1)

withFmap :: Integer -> [Char]
withFmap = reverse.show <$> (+1)