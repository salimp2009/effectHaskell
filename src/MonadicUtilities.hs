module MonadicUtilities where

-- |orginial mapM similar to map
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
mymapM' :: Monad m => (t -> m a) -> [t] -> m [a]
mymapM' _ [] = return []
mymapM' f (a:as) = (:) <$> f a <*> mymapM' f as                    
