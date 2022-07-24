{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
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