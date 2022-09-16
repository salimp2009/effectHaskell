{-# LANGUAGE TemplateHaskell #-}
module Main where

import Lib ()
import GreetingFunction ( greeting )
import TempHaskellProjector
import TemplateHaskellPredicates (mkPredicates)

$(mkProjectors  [2..10])

data Shape = Circle Double
           | Square Double
           | Triangle Double Double Double

$(mkPredicates ''Shape)

main :: IO ()
main = do
    print greeting
    putStrLn $ $(proj 3 1) (undefined,"Success!",undefined) 
    putStrLn $ $(proj 3 2) (undefined,"Success!","2nd success")
    putStrLn $ $(proj 4 2) (undefined,undefined,"Success!",undefined)
    putStrLn $ $(proj 5 4) (undefined,undefined,undefined,undefined,"Success!")
    
    putStrLn $ proj_3_1 (undefined,"Success!",undefined)
    putStrLn $ proj_4_2 (undefined,undefined,"Success!",undefined)
    putStrLn $ proj_5_4 (undefined,undefined,undefined,undefined, "Success!")
    mapM_ print [isCircle s1, isSquare s2, isTriangle s3]
        where
            s1 = Circle 4
            s2 = Square 10
            s3 = Triangle 1 1 1