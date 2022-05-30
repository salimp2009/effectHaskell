module BasicTypes where

-- | basic example to show how type annotations can be added
-- to let and where bindings
calculateTotalCost :: Int -> Int
calculateTotalCost basePrice = 
    let
        priceWithServiceFee :: Int
        priceWithServiceFee = basePrice  + 20
        customaryTip = 6 :: Int
    in priceWithServiceFee + customaryTip

newaddOne :: Int -> Int
newaddOne = (+ 1)

newaddThreeNums :: Int -> (Int -> (Int -> Int))
newaddThreeNums x y z = 
    let 
        f:: Int -> (Int -> (Int -> Int))
        f a =
            let 
                g :: Int -> (Int -> Int)
                g b =
                  let 
                    h :: Int -> Int
                    h c = a + b + c 
                  in h
            in g 
    in f x y z