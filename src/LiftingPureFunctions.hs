module LiftingPureFunctions where
import Control.Monad (ap)

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

-- use case;
-- >>> fmap' (+1) (Just 1)            
-- Just 2
fmap' :: Monad m => (a -> b) -> m a -> m b
fmap' f   = ap' (pure f) 
