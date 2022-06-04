{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TypeHoleUses where

import Data.Function ((&))

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

showStringPair::(String, String) -> String
showStringPair (a,b) = "fst: " <> a <> ", snd: " <> b

doubleField ::a -> (a, a)
doubleField a = (a,a)

showValues::String
showValues = unlines $ map (showStringPair . doubleField . show ) [1..10]

-- | Using type hole to determine the type; original doubleField accepts
-- any type since it is Num, Enum it will not work showStringPair
-- when used with the type hole compiler show exact problem and how to fix it
-- showValues = unlines $ map (showStringPair . _doubleField ) [1..10]

mySwap :: (a , b) -> (b , a)
mySwap (a,b) = (b,a)

myConcat :: [[a]] -> [a]
myConcat = foldr (<>) []

-- | myBuild is build in GHC implementation
myConcat' :: forall a t1 t2. (Foldable t1, Foldable t2) => t1 (t2 a) -> [a]
myConcat'  xs = myBuild (\c n -> foldr (\x y -> foldr c y x) n xs)
    where
      myBuild::((a -> [a] -> [a]) -> [a1] -> t) -> t
      myBuild g = g (:) []

mapApply :: [a -> b] -> [a] -> [b]
mapApply toApply =
    concatMap (\input -> map ($ input) toApply)


example::[Int] -> String
example = mapApply $ (lookupLetter .) <$> offsets
          -- mapApply $ map (lookupLetter .) offsets
          --((lookupLetter .) <$> offsets <*>) 
        where
          letters :: [Char]
          letters = ['a'..'z']

          lookupLetter :: Int -> Char
          lookupLetter n = letters !! n

          offsets :: [Int -> Int]
          offsets = [rot13, swap10, mixupWovels]

          rot13 :: Int -> Int
          rot13 n = (n+13) `rem` 26

          swap10 :: Int -> Int
          swap10 n
              | n <= 10 = n +10
              | n <= 20 = n-10
              | otherwise = n

          mixupWovels n =
              case n of
                0 -> 8
                4 -> 14
                8 -> 20
                14 -> 0
                20 -> 4
                n' -> n'


