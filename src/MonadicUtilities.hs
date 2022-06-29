{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
module MonadicUtilities where

import Control.Monad (forM)

-- |orginal mapM similar to map
-- instead the result is Monad List
-- use case;
-- myMapM  (\name -> print ("Hello, " ++ name)) ["Salitos", "didokitos"]
--"Hello, Salitos"
-- "Hello, didokitos"
-- [(),()]

myMapM :: Monad m => (a -> m b) -> [a] -> m [b]
myMapM _ [] = return []
myMapM f (x:xs) = do r  <- f x
                     rs <- myMapM f xs
                     return (r:rs)

mymapM :: Monad m => (a -> m b) -> [a] -> m [b]                     
mymapM f  = foldr k (return [])
    where
        k a r = do x  <- f a
                   xs <- r 
                   return (x : xs) 
-- | use case;
-- >>> mymapM'  (\name -> (Just ("Hello, " ++ name))) ["Salitos", "didokitos"]
-- Just ["Hello, Salitos","Hello, didokitos"]
mymapM' :: forall (m :: * -> *) t a. Monad m => (t -> m a) -> [t] -> m [a]
mymapM' _ [] = return []
mymapM' f (a:as) = (:) <$> f a <*> mymapM' f as

-- | forM is is similar to mapM but reverses the order of input
-- List first @[a]@ then the function  @(a -> m[b])@
-- use case ;
-- reverseMapM ["Salitos", "didokitos"]  ->
-- >> Hello, "Salitos"
-- >> Hello, "didokitos"
-- >> [(),()]   
reverseMapM :: forall (t :: * -> *) a. (Traversable t, Show a) => t [a] -> IO (t ())
reverseMapM xs= forM xs  $ \name -> putStrLn ("Hello, " ++ show name)

-- | similar sequence in GHC
-- a list of Monad a' s turn into  a Monad of list of a's 
-- move the monadic context from inside the list, '[m a]', to the outside, 'm [a]':
mysequence :: Monad m => [m a] -> m [a]
mysequence [] = return []
mysequence (x : xs) = do  x' <- x
                          rs <- mysequence xs
                          return (x' : rs) 

-- | same as above but using Applicative style 
-- can be writen with foldr as mapM                         
mysequence' :: Monad m => [m a] -> m [a]
mysequence' [] = return []
mysequence' (x:xs) = (:) <$> x <*> mysequence' xs

-- | writing mapM in terms of sequence
mapM' :: Monad m => (a1 -> m a2) -> [a1] -> m [a2]
mapM' f = mysequence . map f