{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeApplications #-}
module ApplicativeBasic where

data List a = Empty |  List a (List a) deriving (Show)
toList:: [a] -> List a
toList = foldr List Empty
-- toList (a:as) = List a (toList as)
-- ^ Original implementation

fromList :: List a -> [a]
fromList Empty = []
fromList (List a as) = a : fromList as

instance Functor List where
  fmap _ Empty = Empty
  fmap f (List a as) = List (f a) (fmap f as)


concatList :: List a -> List a -> List a
concatList Empty bs = bs
concatList (List a as) bs   = List a (concatList as bs)

-- | use case ;
-- funcList = toList [id, succ, (*2)]
-- myList1 = toList [1..5]
-- fromList $  funcList <*> myList1  
-- -> [1,2,3,4,5,2,3,4,5,6,2,4,6,8,10]
instance Applicative List where
  pure x = List x Empty
  Empty <*> _ = Empty
  List f fs <*> bs = (f <$> bs) `concatList` (fs <*> bs)
      
-- a wrapper to work on left side of Either only
newtype ReverseEither a b = ReverseEither (Either b a) 
        deriving (Show)
    
    -- | fmap will pass variable b and that is applied to Left of Either
    -- normally you would right Either a b and apply b to Righ side;
instance Functor (ReverseEither a) where
        fmap f (ReverseEither (Left b))  = ReverseEither (Left (f b))
        fmap f (ReverseEither (Right a)) = ReverseEither (Right a)
    

-- | example to implement '(->)' which works as '(a -> b)'
newtype MFunction a b = MFunction { runFunction :: a -> b }
    

-- | fmap instance for MFunction is similar to 
-- fmap :: (b -> c) -> MFunction a b -> MFunction c
-- fmap :: (b -> c) -> (a -> b) -> (a -> c)
-- similar to (.) :: (b -> c) -> (a -> b) -> (a -> c)
-- since fmap operates on the second type parameter 'b'
-- it turns out to be 
instance Functor (MFunction a) where
  fmap f (MFunction g) = MFunction (f.g) 

-- | implementation of '(<*>)' ;
-- (<*>) :: f (a->b) -> f a -> f b
-- (<*>) :: Function a (b->c) -> Function a b -> Function a c  
-- (<*>) :: (a->b->c) -> (a -> b) -> (a -> c)  
instance Applicative (MFunction a) where
 pure a = MFunction $ const a
 MFunction f <*> MFunction g = MFunction $ \a -> f a (g a) 

-- | as an example of the case MFunction
-- zipWith has a type signature 
-- (a -> b -> c) -> [a] -> [b] -> [c]  
-- and zipWith (+) has a type signature
-- [c] -> [c] -> [c]
-- so it is similar to [ (+)] <*> [a] <*> [a]
fibsApplic :: [Integer]
fibsApplic = 0 : 1 : (zipWith (+) <*> tail) fibsApplic

-- | it applies given value to the function on the right
-- and also it applies the given value to the right and result 
-- from left side is also applied
-- the function on the right side is a binary function
--use case ;
-- >>> myval 10
-- 200
myval :: Integer -> Integer
myval = runFunction $ MFunction (*) <*> MFunction (*2)

-- | pure applies Applicative to a given value
-- in this case pure 5 gives us a Function that returns 5  '(_ -> 5)'
-- use case;
-- >>> myval2 30
-- 150
myval2 :: Integer -> Integer
myval2 = runFunction $ MFunction (*) <*> pure 5
