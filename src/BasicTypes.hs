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