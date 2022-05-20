
module ListComprehend where

myDouble :: [Integer]
myDouble = [ num *2 | num <- [0..10] ]

doubleOdds :: [Integer]
doubleOdds = [ num *2 | num <- [0..10], num < 5 , odd num ]

doubleOdds' :: [Integer]
doubleOdds' = map (*2) . filter odd $ [1..15]

pairs :: Integral a => [a] -> [a] -> [(a, a)]
pairs as bs =
    let as' = filter (`elem` bs) as
        bs' = filter odd bs
    in  (,) <$> as' <*> bs'

pairs'' :: Integral b => [b] -> [b] -> [(b, b)]
pairs'' as bs = [(a, b) | a <- as, a `elem` bs, b <- bs, odd b]


romanNums :: [(Integer, String)]
romanNums = [(num, str) | num <-[1,2,3], str <- ["I", "II", "III", "IV"] ]

combineLists :: [a1] -> [a2] -> [(a1, a2)]
combineLists as bs
    | null as || null bs = []
    | otherwise =
            let
                a = head as
                b = head bs
                as' = tail as
                bs' = tail bs
            in (a,b) : combineLists as' bs'

combineLists'' :: [a] -> [b] -> [(a, b)]
combineLists'' = zip

pairwiseSum :: Num b => [b] -> [b] -> [b]
pairwiseSum xs ys =
    let sumElems pairs =
            let a = fst pairs
                b = snd pairs
            in a + b
    in map  sumElems $ zip xs ys

pairwiseSum' :: Num c => [c] -> [c] -> [c]
pairwiseSum' xs ys =
    let sumElems pairs =
            let a = fst pairs
                b = snd pairs
            in a + b
    in zipWith (curry sumElems) xs ys

pairwiseSum'' :: Num c => [c] -> [c] -> [c]
pairwiseSum'' xs ys = map (uncurry (+))  $ zip xs ys

pairwiseSum''' :: Num c => [c] -> [c] -> [c]
pairwiseSum''' = zipWith (+) 