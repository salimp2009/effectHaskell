module Lib where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

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

-- a wrapper to work on left side of Either only
newtype ReverseEither a b = ReverseEither (Either b a) 
    deriving (Show)

-- | fmap will pass variable b and that is applied to Left of Either
-- normally you would right Either a b and apply b to Righ side;
instance Functor (ReverseEither a) where
    fmap f (ReverseEither (Left b))  = ReverseEither (Left (f b))
    fmap f (ReverseEither (Right a)) = ReverseEither (Right a)

addOne :: Num a => a -> a
addOne n = n + 1

addOneShow :: Integer -> String
addOneShow = show <$> addOne

addOneShowReverse :: Integer -> [Char]
addOneShowReverse = reverse <$> addOneShow

withComposition :: Integer -> [Char]
withComposition = reverse.show.(+1)

withFmap :: Integer -> [Char]
withFmap = reverse.show <$> (+1)

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

-- | implementation of '(<*>)' ;
-- (<*>) :: f (a->b) -> f a -> f b
-- (<*>) :: Function a (b->c) -> Function a b -> Function a c  
-- (<*>) :: (a->b->c) -> (a -> b) -> (a -> c)  
instance Applicative (MFunction a) where
  pure a = MFunction $ const a
  MFunction f <*> MFunction g = MFunction $ \a -> f a (g a)