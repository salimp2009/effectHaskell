module FoodBudget where
import FoldFunctions ( myFoldr, myFoldl )

checkGuestList :: (Foldable t, Eq a) => t a -> a -> Bool
checkGuestList guestList name = name `elem` guestList
-- ^ elem = any . (=) in the GHC 

foodCosts :: [(String, Double)]
foodCosts = [ ("Sal", 10.00)
             ,("Dido", 25.00)
             ,("Semo", 27.50)
             ,("Demir", 30.00)
            ]

checkGuestList' :: (Foldable t, Eq a) =>  a -> t a -> Bool
checkGuestList' = elem


partyBudget :: Num c => (a -> Bool) -> [(a, c)] -> c
partyBudget isAttending =
    myFoldr (+) 0 . map snd . filter (isAttending . fst)

partyBudget' :: Double
partyBudget' =
     myFoldr (+) 0
     . map snd
     . filter (\namePrice -> fst namePrice `elem` ["Sal", "Semo"]) $ foodCosts

-- | Example to show how list comprehension used to throw away one of variables; 
--   eg. guest is not used in the final list but used to constrain and calculate
--   the food cost of guests are eating :)        
partyBudget'' :: Num a =>(t1 -> Bool) -> (t1 -> t2 -> Bool) -> (t2 -> a) -> ([t1], [t2]) -> a
partyBudget'' isAttending willEat foodCost guests =
        myFoldl (+) 0 $ 
        [ foodCost food 
        | guest <- fst guests
        , food  <- snd guests
        , willEat guest food
        , isAttending guest 
        ]