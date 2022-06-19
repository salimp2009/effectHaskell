{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module StateContext where

data Tree a = Leaf a | Node (Tree a) (Tree a)
        deriving Show

myTreeData1 :: Tree Char
myTreeData1=  Node (Node (Leaf 'x') (Leaf 'y')) (Leaf 'z')

numberofTreeLeaves :: Tree a -> Int
numberofTreeLeaves (Leaf a)  = 1
numberofTreeLeaves (Node l r) = numberofTreeLeaves l + numberofTreeLeaves r

numberofNodes :: Tree a -> Int
numberofNodes (Leaf a) = 0
numberofNodes (Node l r) = 1 + numberofNodes l + numberofNodes r

-- | add index to each Leaf and the Leaf on the right also 
-- return the next index; rather have Tree (Int, a)
-- this return ()
relabelTree :: Tree a -> Int -> (Tree (Int, a), Int)
relabelTree (Leaf x) i = (Leaf(i, x), i+1)
relabelTree (Node l r) num = 
        let (l', i1) = relabelTree l num
            (r', i2) = relabelTree r i1
        in  (Node l' r', i2)
        
stripTree :: (Tree (Int, a), Int) -> Tree (Int, a)
stripTree (Leaf (i, x), i2) = Leaf (i, x)
stripTree (Node l r, i2) = Node (stripTree (l, i2)) (stripTree (r, i2))          

