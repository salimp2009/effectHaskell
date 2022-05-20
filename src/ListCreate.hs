module ListCreate where

import FoldFunctions ( myFoldr )

listIsEmpty :: [a] -> String
listIsEmpty list =
    case list of
        [] -> "this list is empty !"
        (_ : _) -> "this list is not empty!"


listIsEmpty'' :: Foldable t => t a -> String
listIsEmpty'' list =
    if null list
        then  "this list is empty !"
        else  "this list is not empty!"


listIsEmpty''' :: (Foldable t, Show (t a)) => t a -> String
listIsEmpty''' list
    | null list = "empty list !!"
    | otherwise = "not empty: " <> show list


--countdown :: (Num a, Enum a) => Int -> [a]
countdown :: Int -> [Int]
countdown n = take n [n, n-1..]

countdown' :: Int -> [Int]
countdown' n
            | n <=0 = []
            | otherwise = n : countdown (n-1)

countdown'' :: Int -> [Int]
countdown'' n =
    if n <=0 then []
    else n: countdown (n-1)

factors :: Integral a => a -> [a]
factors num = 
      factors' num 2
      where 
          factors' num fact
            | num == 1 = []  
            | (num `rem` fact) == 0 = fact : factors' (num `div` fact) fact
            | otherwise = factors' num (fact + 1)

isBalanced :: [Char] -> Bool
isBalanced s = 
    0 == isBalanced' 0 s
    where isBalanced' count s
            | null s = count
            | head s == '(' = isBalanced' (count + 1) (tail s)
            | head s == ')' = isBalanced' (count -1 ) (tail s)
            | otherwise = isBalanced' count (tail s)

reduce :: (t -> a -> t) -> t -> [a] -> t
reduce func carryValue lst 
    | null lst = carryValue
    | otherwise =
        let intermediateValue = func carryValue (head lst)
        in  reduce func intermediateValue (tail lst)

isBalanced'' :: [Char] -> Bool
isBalanced'' str = 0 == reduce checkBalance 0 str
        where 
        checkBalance count letter
            | letter == '(' = count + 1
            | letter == ')' = count - 1
            | otherwise = count

doubleElems :: Num a => [a] -> [a]
doubleElems nums 
        | null nums = []
        | otherwise = 
            let hd = head nums
                tl = tail nums
            in (*2) hd : doubleElems tl
            
map' :: (a1 -> a2) -> [a1] -> [a2]
map' f xs 
        | null xs = []
        | otherwise = f (head xs) : map' f (tail xs)

doubleElems' :: [Integer] -> [Integer]
doubleElems' = myFoldr doubleElem []
        where
            doubleElem num lst = (2 * num ) : lst

doubleElems'' :: [Integer] -> [Integer]
doubleElems'' = myFoldr (applyElem (*2)) []
        where
            applyElem f elem accum = f elem : accum