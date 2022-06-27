--{-# LANGUAGE DerivingStrategies    #-}
--{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingVia #-}

module LiftingPureFunctions where
import Control.Monad (ap)
import Control.Applicative ( ZipList(..), getZipList )

plus' :: Maybe Int -> Maybe Int -> Maybe Int
plus' x y = do a <- x
               b <- y
               return (a + b)

plus''  :: Maybe Int -> Maybe Int -> Maybe Int
plus'' x y = (+) <$> x <*> y


lift2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
lift2 f x y = do a <- x
                 b <- y
                 return (f a b)

lift2' :: Monad m => (a -> b -> c) -> m a -> m b -> m c
lift2' f x y = f <$> x <*> y

lift3' :: Monad m => (a -> b -> c -> d) -> m a -> m b -> m c -> m d
lift3' f x y z= f <$> x <*> y <*> z

ap' :: Monad m => m (a -> b) -> m a -> m b
ap' f x = do f' <- f
             a  <- x
             return (f' a)

ap'' :: Monad m => m (a -> b) -> m a -> m b
ap'' f x = do f' <- f
              f' <$> x        

-- use case;
-- >>> fmap' (+1) (Just 1)            
-- Just 2
fmap' :: Monad m => (a -> b) -> m a -> m b
fmap' f   = ap' (pure f)

-- | using deriving Functor and Applicative via ZipList
-- which means we use the same implementation with ZipList
-- since the underlying types are same 
newtype ZipListF a = ZipListF { getZipListF :: [a]}
        deriving Show
        deriving (Functor, Applicative) via ZipList

-- | regular List to create a tuple
-- use case;
-- >>> zippedLists
-- [(1,3),(1,4),(2,3),(2,4)]
zippedLists :: [(Integer, Integer)]
zippedLists = fmap (,) [1,2] <*> [3,4]   

-- | ZipList to create a tuple has a different implementation
-- ZipList is a regular `[a]` but has a different Applicative instance
-- for @(<*>)@ and @fmap@ it takes each element from each list
-- where as regular list takes the first element from first list 
-- and consume all elements of the 2nd list then goes to next element 
-- of the first ; it is like nested @do@ blocks
-- use case;
-- >>> zippedWithZipList
-- [(1,300),(2,400)]
zippedWithZipList :: [(Integer, Integer)]
zippedWithZipList = getZipList $ fmap (,) (ZipList [1,2]) <*> ZipList [300,400]

-- use case;
-- >>> zippedWithZipListF
-- [(1,300),(2,400)]
zippedWithZipListF :: [(Integer, Integer)]
zippedWithZipListF = getZipListF $ fmap (,) (ZipListF [1,2]) <*> ZipListF [300,400]

-- | use case;
-- >>> zippedWithZipListF'
-- [(1,300),(2,400)]
zippedWithZipListF' :: [(Integer, Integer)]
zippedWithZipListF' = getZipListF $ (,) <$> ZipListF [1,2] <*> ZipListF [300,400]