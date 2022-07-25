{-# LANGUAGE RankNTypes #-}
--{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
module RankNTypeExamples where

-- | this does not work because
-- for all a to a we can not get Int
-- we can only get Int if a is Int therefore
-- GHC rejects it
-- applytoFive :: forall a. (a -> a) -> Int
-- applytoFive f = f 5

-- | this works because for top level we dont have forall a
-- it applies to function f and we instantiate with Int it always give an
-- Int; f is rank 1 and applytoFive2 function is rank-2
applytoFive2 :: (forall a. a -> a) -> Int
applytoFive2  f = f 5

-- | this works because by having ScopeTypeVariable
-- the top level definition of a applies to function f since a 
-- appears only in (a -> a)
applytoFive3 :: forall a. (a -> a) -> Int
applytoFive3  f = f 5
        where
            f :: forall a. a -> a
            f = id

-- | the rank of a function is simply the number of arrows its deepest
-- forall is to the left of.

applytoFive4 :: (Num t1, Num t2) => (t1 -> t2) -> t2
applytoFive4 f = f 5

-- | deepest forall is forall t1
-- the arrows to left of forall t1 is 2
-- so the rank is 2
applytoFive5 :: forall t2. (forall t1. (t1 -> t2)) -> t2
applytoFive5 f = f 5

-- | rank is 1; there is 1 arrow after deeper forall; forall a...
-- same as 
-- rankTest1 :: forall a. Int -> a -> a
rankTest1 :: Int -> (forall a. a -> a)
rankTest1 a = id

-- | rank 2 ; there are 2 arrows after deeper forll ; forall c...
rankTest2 :: forall a b. (a -> b) -> (forall c. c -> a) -> b
rankTest2 f g = undefined

-- | rank 3; there are 3 arrows after the deeper forall; forall x...
rankTest3 :: ((forall x. m x -> b (z m x)) -> b(z m a)) -> m a
rankTest3 f = undefined

-- | a and (forall r. (a -> r) -> r) are isomorphic
-- Identity a and a are isomorphic
-- since isomorphism is transistive 
-- Identity a and (forall r. (a -> r) -> r) is isomorphic
-- (forall r. (a -> r) -> r) known as CPS (Continuation-Passing Style)
-- Identity a is Monad so we expect CPS to be a Monad as well
cont :: a -> (forall r. (a -> r) -> r)
cont a = \callback -> callback a

runCont :: forall a. (forall r. (a -> r) -> r) -> a
runCont f =
    let
        callback = id
    in f callback

newtype Cont a = Cont
    { unCont :: forall r. (a -> r) -> r }

instance Functor Cont where
    fmap f (Cont c) = Cont $ \c' -> c (c' . f)
    -- fmap f (Cont c) = Cont $ \c' -> let r' = c'.f in c r'  

instance Applicative Cont where
    pure x = Cont $ \c -> c x
    (Cont f) <*>  (Cont a) = Cont $ \br ->
                                f $ \ab ->
                                a $ br . ab

instance Monad Cont where
    --(>>=)       :: forall a b. m a -> (a -> m b) -> m b                                
    (Cont m) >>= k = Cont $ \c ->
                        m $ \a ->
                            unCont (k a) c
    -- m >>= k = Cont $ \c ->
    --     unCont m (\a -> unCont (k a) c)

-- | classic way  JavaScript-style “pyramids of doom.”
-- Cont Monad instance will be used later to flatten it
withVersionNumber :: (Double -> r) -> r
withVersionNumber f = f 1.0

withTimestamp :: (Int -> r) -> r
withTimestamp f = f 1532083362

withOS :: (String -> r) -> r
withOS f = f "linux"

-- | use case;
-- >>>releaseSTring 
-- "linux-1.01532083362"
releaseSTring :: String
releaseSTring =
    withVersionNumber $ \version ->
        withTimestamp $ \date    ->
            withOS $ \os         ->
                os <> "-" <> show version <> show date

-- | refactor above with Cont (or ContT)
releaseStringCont :: String
releaseStringCont = runCont $ unCont $ do
    version <- Cont withVersionNumber
    date    <- Cont withTimestamp
    os      <- Cont withOS
    pure $ os <> "-" <> show version <> show date

newtype ContT2 r m a = ContT2 { runContT2 :: (a -> m r) -> m r}

-- | "The result of running a CPS computation with 'return' as the
-- final continuation" GHC implementation (for learning purposes)
evalContT2 :: Monad m => ContT2 r m r -> m r
evalContT2 m = runContT2 m return
{-# INLINE evalContT2 #-}

instance Functor (ContT2 r m) where
    fmap f m = ContT2 $ \c -> runContT2 m ( c . f)

instance Applicative (ContT2 r m) where
    --pure x = ContT2 ($ x)
    pure x = ContT2 $ \c -> c x 
    -- <*> :: ContT2 r m (a -> b) -> ContT2 r m a -> ContT2 r m b
    f <*> g = ContT2 $ \c -> runContT2 f $ 
                       \b -> runContT2 g (c . b)

instance Monad (ContT2 r m) where  
    return x = ContT2 ($ x)
    -- >>= :: ContT2 r m a -> (a -> ContT2 r m b) -> ContT2 r m b
    m >>= f = ContT2 $ \c -> runContT2 m (\a -> runContT2 (f a) c)