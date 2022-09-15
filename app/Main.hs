{-# LANGUAGE TemplateHaskell #-}
module Main where

import Lib ()
import GreetingFunction ( greeting )
import TempHaskellProjector


$(mkProjectors  [2..10])

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