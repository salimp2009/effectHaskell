{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
module RankNTypeExamples where

applytoFive2 :: (forall a. a -> a) -> Int
applytoFive2  f = f 5

applytoFive :: forall a. (a -> a) -> Int
applytoFive  f = f 5 
        where
            --f :: forall a. a -> a
            f = id
