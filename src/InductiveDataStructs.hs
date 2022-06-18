
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}

module InductiveDataStructs where

data Peano = Z | MS Peano
    deriving (Show)

-- | use case ; toPeano 3 -> MS (MS (MS Z))    
toPeano :: Int -> Peano
toPeano 0 = Z
toPeano n = MS (toPeano $ n-1)

-- | use case; fromPeano (MS (MS (MS Z))) -> 3
-- it recursively calls fromPeano function until it reaches Z
-- every call succ add 1 once it is called it adds 1 and sends 
-- result back to upper call then add 1+1 ->2 ; end at total = 3 in this case
fromPeano :: Peano -> Int
fromPeano  Z = 0
fromPeano (MS p) = succ (fromPeano  p)

-- | if any of the values; 
--    (Z, MS _) -> False
-- or (MS _, Z) -> False
eqPeano :: Peano -> Peano -> Bool
eqPeano  p p' =
    case (p, p') of
        (Z, Z) -> True
        (MS n, MS n') -> eqPeano n n'
        _ -> False

-- | test values
myPeano1 :: Peano
myPeano1 = MS (MS (MS Z))

myPeano2 :: Peano
myPeano2 = MS (MS Z)

-- | we remove first parameter from the one of the values
-- and add one to the other; once the removed value reach zero
-- we return the added value ; so the amount we add equals to amount we removed
-- similar to (+1)
addPeanos :: Peano -> Peano -> Peano
addPeanos Z b = b
addPeanos (MS a) b = addPeanos a (MS b)


data MList a = MEmpty | Cons a (MList a)
    deriving Show

toMList :: [a] -> MList a
toMList = foldr Cons MEmpty

fromMList :: MList a -> [a]
fromMList MEmpty = []
fromMList (Cons x xs) = x : fromMList xs

fromMList' :: MList a -> [a]
fromMList'  = listfoldr (:) []

listfoldr :: (a -> b -> b) -> b -> MList a -> b
listfoldr _ b MEmpty = b
listfoldr f b (Cons x xs) =
        f x $ listfoldr f b xs

listfoldl :: (b -> a -> b) -> b -> MList a -> b
listfoldl _ b MEmpty = b
listfoldl f b (Cons x xs) =
        let intermediateVal = f b x
        in listfoldl f intermediateVal xs

-- use case; 
-- listHead myMList -> Just 1       
listHead :: MList a -> Maybe a
listHead MEmpty = Nothing
listHead (Cons x xs) = Just x

-- use case; 
-- listTail myMList -> Cons 2 (Cons 3 (Cons 4 (Cons 5 MEmpty)))
listTail :: MList a -> MList a
listTail MEmpty = MEmpty
listTail (Cons _ xs) = xs

-- | use case;
-- fromlistReverse myMList -> [5,4,3,2,1]
fromlistReverse :: MList a -> [a]
fromlistReverse = listfoldl (flip (:)) []

-- use case ;
-- listReverse myMList -> Cons 5 (Cons 4 (Cons 3 (Cons 2 (Cons 1 MEmpty))))
listReverse :: MList a -> MList a
listReverse = listfoldl (flip Cons) MEmpty

-- | use case ; 
-- listMap' (*20) myMList -> 
--  Cons 20 (Cons 40 (Cons 60 (Cons 80 (Cons 100 MEmpty))))
listMap :: (a -> b) -> MList a -> MList b
listMap _ MEmpty = MEmpty
listMap f (Cons x xs) = Cons (f x) (listMap f xs)

-- | use case ; 
-- listMap' (*20) myMList -> 
--  Cons 20 (Cons 40 (Cons 60 (Cons 80 (Cons 100 MEmpty))))
listMap' :: (a -> b) -> MList a -> MList b
listMap' f = listfoldr g MEmpty
    where
        g x   = Cons (f x)

-- | test data for MList a
myMList :: MList Integer
myMList = toMList [1..5]

data BinaryTree a = Leaf | Branch (BinaryTree a) a (BinaryTree a)
        deriving (Eq, Show)

binaryTree1 :: Num a => BinaryTree a
binaryTree1 = Branch (Branch Leaf 2 (Branch Leaf 6 Leaf) ) 1 (Branch (Branch Leaf 4 Leaf) 3 (Branch Leaf 5 Leaf) )

-- indent :: [String] -> [String]
-- indent = map ("  "<>)

showStringTree :: Show a => BinaryTree a -> [String]
showStringTree Leaf =  []
showStringTree (Branch ls m rs) = indent' (showStringTree ls) <> [show m] <> indent' (showStringTree rs)
        -- | added later Test this if it works
        where 
            indent' :: [String] -> [String]
            indent' = map ("  "<>)

prettyTree :: Show a => BinaryTree a -> String
prettyTree = unlines . showStringTree

numberofLeaves :: forall a. BinaryTree a -> Int
numberofLeaves Leaf  = 1
numberofLeaves (Branch ls m rs) = numberofLeaves ls + numberofLeaves rs

addInttoTree :: Int -> BinaryTree Int -> BinaryTree Int
addInttoTree number Leaf = Branch Leaf number Leaf
addInttoTree number tree@(Branch ls m rs) 
    | number < m = Branch (addInttoTree number ls) m rs
    | number > m = Branch ls m (addInttoTree number rs)
    | otherwise = tree

