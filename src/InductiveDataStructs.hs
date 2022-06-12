
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

listHead :: MList a -> Maybe a
listHead MEmpty = Nothing
listHead (Cons x xs) = Just x

listTail :: MList a -> MList a
listTail MEmpty = MEmpty
listTail (Cons _ xs) = xs

fromlistReverse :: MList a -> [a]
fromlistReverse = listfoldl (flip (:)) []








