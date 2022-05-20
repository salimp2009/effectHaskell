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
    fmap f (ReverseEither (Left a))  = ReverseEither (Left (f a))
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

