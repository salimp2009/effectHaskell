module DoNotation where

import StateContext ( WithCounter 
                    , Tree(..) 
                    , validatePerson
                    , validateName
                    , validateAge
                    , Person(..) 
                    )

-- | Monadic interface for the Tree
-- only for example; need implement Monad instance of it 
-- to be able to use this 
-- relabelR :: Tree a -> WithCounter (Tree (Int, a))
-- relabelR (Leaf x) = \i -> (Leaf (i ,x), i +1)
-- relabelR (Node l r) = relabelR l >>= \l' ->
--                       relabelR r >>= \r' ->
--                       return (Node l' r')  

-- | example for using Monad interface
-- @(>>=)@ bind function passes the value inside Maybe a
-- to a function @(a ->Maybe)@ 
-- return or pure takes the given value and puts into Monad ; (a -> ma)
-- use case;
-- >>> validatePersonR "Salitos" 24  
-- Just (Person {name = "Salitos", age = 24})
validatePersonR :: String -> Int -> Maybe Person
validatePersonR name age 
    = validateName name >>= \name' ->
      validateAge age   >>= \age'  ->
      return (Person name' age')    

