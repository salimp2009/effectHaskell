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
-- use case ;
-- >>> relabelTree myTreeData1 0
-- (Node (Node (Leaf (0,'x')) (Leaf (1,'y'))) (Leaf (2,'z')),3)
relabelTree :: Tree a -> Int -> (Tree (Int, a), Int)
relabelTree (Leaf x) i = (Leaf(i, x), i+1)
relabelTree (Node l r) num =
        let (l', i1) = relabelTree l num
            (r', i2) = relabelTree r i1
        in  (Node l' r', i2)

-- | use case ;
-- >>> getRelabeledTree myTreeData1 0        
-- Node (Node (Leaf (0,'x')) (Leaf (1,'y'))) (Leaf (2,'z'))
getRelabeledTree :: Tree a -> Int -> Tree (Int, a)
getRelabeledTree tree index= fst $ relabelTree tree index

type WithCounter a = Int -> (a, Int)

next :: WithCounter a -> (a -> WithCounter b) -> WithCounter b
f `next` g  =  \i -> let (r, i') = f i in g r i'

pure' :: a -> WithCounter a
pure' x i = (x, i)

relabel :: Tree a -> WithCounter (Tree (Int, a))
relabel (Leaf x) = \i -> (Leaf (i, x), i+1)
relabel (Node l r) = relabel l `next` \l' ->
                     relabel r `next` \r' ->
                     pure' (Node l' r')

