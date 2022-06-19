{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module StateContext where

data Tree a = Leaf a | Node (Tree a) (Tree a)
        deriving Show

myTreeData1 :: Tree Char
myTreeData1=  Node (Node (Leaf 'x') (Leaf 'y')) (Leaf 'z')