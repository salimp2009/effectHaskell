module InductiveDataStructs where

data Peano = Z | MS Peano 
    deriving Show

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
fromPeano (MS p) = succ (fromPeano p) 