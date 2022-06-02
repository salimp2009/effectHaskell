module TypeHoleUses where

exampleNums :: [Int]
exampleNums = [1..10]

-- | this wont compile and error due to type hole after quantity _
-- the error messages will tell us what type needed to work; a tech that can be used
-- if we cant figure out the type 
    
-- getFiveNUms :: [Int]
-- getFiveNUms = let quantity = 5 in take quantity _

permuteThruple :: (a,b,c) -> ((a,b,c), (a,c,b), (b,a,c),(b,c,a),(c,a,b),(c,b,a))
permuteThruple (a,b,c) = ((a,b,c), (a,c,b), (b,a,c),(b,c,a),(c,a,b),(c,b,a))

mergeFirstTwo :: (a,b,c) ->(a->b->d) -> (d,c)
mergeFirstTwo (a, b, c) f = (f a b , c)

showFields :: String
showFields = 
    let (a,b) = combinePermutations . permuteThruple $ ("hello", "world", 10)
    in unlines [fst a , fst b]
    where
        joinFields a b = show a <> " - " <> b
        combinePermutations (a,b,c,d,e,f) = 
            ( mergeFirstTwo a joinFields, 
              mergeFirstTwo e joinFields
            )

