{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- {-# LANGUAGE DeriveAnyClass #-}
-- {-# LANGUAGE DeriveFunctor #-}


module StateContext where

import Data.Char (isAlphaNum)

data Tree a = Leaf a | Node (Tree a) (Tree a)
        deriving (Show)

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

type State s a = s -> (a , s)

pureS :: a -> State s a
pureS x s = (x, s)

nextS :: State s a -> (a -> State s b) -> State s b
f `nextS` g = \s -> let (a, s') = f s in g a s'

appendLists :: [a] -> [a] -> [a]
appendLists [] ys = ys
appendLists (x : xs) ys = x : xs ++ ys

type Name = String

data Person = Person
             { name :: Name
             , age  :: Int
             } deriving Show

validateName :: String -> Maybe Name
validateName name
                | all isAlphaNum name = Just name
                | otherwise = Nothing

validateAge :: Int -> Maybe Int
validateAge age
                | age >= 0 = Just age
                | otherwise = Nothing
-- | this solution does not scale if there are
-- are additional validation requirements to be added
validatePerson :: String -> Int -> Maybe Person
validatePerson name age =
        case validateName name of
          Nothing -> Nothing
          Just name' -> case validateAge age of
                          Nothing -> Nothing
                          Just age' -> Just (Person name' age')

-- | this is equivalent to v >>= g in Monad functions
then_ :: Maybe a -> (a -> Maybe b) -> Maybe b
then_ v g = case v of
                Nothing -> Nothing
                Just v' -> g v'


-- | using monad like functions to chain
validatePerson' :: String -> Int -> Maybe Person
validatePerson' name age =
        validateName name `then_` \name' ->
        validateAge age  `then_`  \age'  ->
        Just (Person name' age')         
-- | use case;
-- >>> flatten (Just (Just 4))        
-- Just 4
flatten :: Maybe (Maybe c) -> Maybe c
flatten oo = then_ oo id

-- | just expanded prev one to prove then_ does the work
-- use case;
-- >>> flatten' (Just (Just 4)) 
-- Just 4
flatten' :: Maybe (Maybe c) -> Maybe c
flatten' oo = case  oo of
                 Nothing -> Nothing
                 Just o' -> id o'

-- rewriting flatten for State s a in terms of then_
-- unwrapping the first State reveal @a which is another State
-- r is the inner State s a                  
flatten'' :: State s (State s a) -> State s a 
flatten'' ss = \i -> let (r , i') = ss i in id r i'
-- ^           \i -> let (r, i) = ss i in r i      -- << same as above           


