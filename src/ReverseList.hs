module ReverseList where

import Data.Monoid (Endo(..), appEndo)

-- | standart GHC definition
myReverse :: Foldable t => t a -> [a]
myReverse = foldl (flip (:)) []

-- | not efficient because of constant concatenate
myReverse' :: [a] -> [a]
myReverse' [] = []
myReverse' (x:xs) = myReverse xs <> [x]

myReverse'' :: [a] -> [a]
myReverse'' list = 
    reverse' list []
    where
        reverse' [] reversed = reversed
        reverse' (x:xs) reversed = reverse' xs (x:reversed) 

-- | Stackoverflow solns; 
myReverse''' :: [a] -> [a]
myReverse''' [] = []
myReverse''' list= foldr (\x fId empty-> fId (x:empty)) id list []

moreReverse :: Foldable t => t a -> [a]
moreReverse xs = foldr funct id xs []
    where 
        funct x fId = fId.(x:)    -- ^ fId will be id -> id.(xn:)

moreReverse' :: Foldable t => t a -> [a]
moreReverse' xs  = ( appEndo . foldr (flip mappend . Endo . (:) ) mempty $ xs ) []