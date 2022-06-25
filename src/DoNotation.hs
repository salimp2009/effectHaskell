--{-# LANGUAGE BlockArguments #-}


module DoNotation where

import StateContext ( WithCounter
                    , Tree(..)
                    , validatePerson
                    , validateName
                    , validateAge
                    , Person(..)
                    , Name
                    , State(..)
                    , relabel
                    )
import Text.Read (readMaybe, read)

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

-- >>> validatePersonR2 "Salitos" 25  
-- Just (Person {name = "Salitos", age = 25})
validatePersonR2 :: String -> Int -> Maybe Person
validatePersonR2 name age
   = do name'  <- validateName name
        age'   <- validateAge age
        return (Person name' age')
-- >>> validatePersonR3 "Salitos" 26  
-- Just (Person {name = "Salitos", age = 26})
validatePersonR3 :: String -> Int -> Maybe Person
validatePersonR3 name age
   =  validateName name >>= \name' ->
      do
        age' <- validateAge age
        return (Person name' age')

data PersonR = PersonR
  { name :: Name
  , age  :: Int
  , underage :: Bool
  } deriving Show

-- >>> validatePersonR4 "Salitos" 26 
-- Just (PersonR {name = "Salitos", age = 26, underage = False})
validatePersonR4 :: String -> Int -> Maybe PersonR
validatePersonR4 name age
  = validateAge age >>= \age' ->
    let uAge = age < 18
     in validateName name >>= \name' ->
       return (PersonR name' age' uAge)

-- | final version of do block with let expression inside
--  Notice in is not used to avoid another do block
-- other option is uAge = return (age <18)
-- use case;
-- >>> validatePersonR5 "Salitos" 26 
-- Just (PersonR {name = "Salitos", age = 26, underage = False})
validatePersonR5 :: String -> Int -> Maybe PersonR
validatePersonR5 name age
  = do age' <- validateAge age
       let uAge = age < 18
       name' <- validateName name
       return (PersonR name' age' uAge)

-- | this is another way of the previos but prefer using let without in       
validatePersonR6 :: String -> Int -> Maybe PersonR
validatePersonR6 name age
  = do age' <- validateAge age
       uAge <- return(age < 18)
       name' <- validateName name
       return (PersonR name' age' uAge)

getNumber :: IO (Maybe Int)
getNumber = do input  <- getLine
               let number = readMaybe input :: Maybe Int
               return number

getNumber' :: IO Int
getNumber' = do n <- getNumber
                case n of
                  Just m  -> return m
                  Nothing -> getNumber'

getNumber'' :: IO ()
getNumber'' = getNumber >>= \e -> case e of
                    Just n  -> putStrLn $ "your number is " <> show n
                    Nothing -> fail "Pattern match failure"                  
      