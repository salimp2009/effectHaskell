{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TupleSections #-}
--{-# LANGUAGE MultiParamTypeClasses #-}
module Main where

import Lib 
import GreetingFunction ( greeting )
import TempHaskellProjector ( proj, mkProjectors )
import TemplateHaskellPredicates (mkPredicates)
import TemplateHaskellQuasiQuoters ( str )
import TemplateHaskLookupTable ( precompute, bigMathProblem )
import TempHaskellQuasiQuoting ( generateTupleBoilerPlate )
import TypedTHPrimesTH ( primesUpTo2' )
import WebAPIServant
import Network.Wai.Handler.Warp 

-- $(generateTupleClass 3)

-- $(generateTupleInstance 3 5)

$(generateTupleBoilerPlate 10)

$(precompute [1..5])


$(mkProjectors  [2..10])

data Shape = Circle Double
           | Square Double
           | Triangle Double Double Double

$(mkPredicates ''Shape)

multStr :: String
multStr = [str| What needs my Shakespeare for his honoured bones,
The labor of an age in pilèd stones,
Or that his hallowed relics should be hid
Under a star-ypointing pyramid?
Dear son of Memory, great heir of fame,
What need’st thou such weak witness of thy name?
Thou in our wonder and astonishment
Hast built thyself a live-long monument…
        |]

--

main :: IO ()
main = do
    print greeting
   
    let numbers = $$(primesUpTo2' 10000)
    putStrLn "Which prime number do you want to know (should be  smaller than ~ 1000)"
    input <- readLn
    if input < length numbers
        then print (numbers !! (input-1))
        else  putStrLn "Number too big"
    
    putStrLn $ $(proj 3 1) (undefined,"1st Success!",undefined)
    putStrLn $ $(proj 3 2) (undefined,"Success!","2nd success")
    putStrLn $ $(proj 4 2) (undefined,undefined,"2nd Success!",undefined)
    putStrLn $ $(proj 5 4) (undefined,undefined,undefined,undefined,"3rd Success!")

    putStrLn $ proj_3_1 (undefined,"1st Success!",undefined)
    putStrLn $ proj_4_2 (undefined,undefined,"2nd Success!",undefined)
    putStrLn $ proj_5_4 (undefined,undefined,undefined,undefined, "3rd Success!")

    print $ _1 (43, "hello", "Demir", [], 3.14)
    print $ _5 (1, 2, 3, 4, 5, 6, 7, 8, 9)
    
    putStrLn multStr

    run 8081 app
 

    mapM_ print [isCircle s1, isSquare s2, isTriangle s3]
        where
            s1 = Circle 4
            s2 = Square 10
            s3 = Triangle 1 1 1