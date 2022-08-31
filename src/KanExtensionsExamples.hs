{-# LANGUAGE TypeApplications #-}

module KanExtensionsExamples where

import Data.Functor.Yoneda
import Data.Functor.Day.Curried
import Control.Monad.Codensity

-- | in order to avoid performance bottlenecks
-- related to be to Generic code
-- e.g: forall m a. ma versus forall a. Maybe a is better
-- using Yoneda will help ; by wrapping our too generic code into Yoneda
-- as less Generic so GHC can inline / optimize and 
-- then convert later to regular form
{- do this instead;
  - forall f. Functor f => f a, instead use
    forall f. Yoneda f a
  - forall f. Applicative f => f a, instead use
    forall f. Curried (Yoneda f) (Yoneda f) a
  - forall f. Monad f => f a, instead use
    forall f. Codensity f a
-}

{- 
  Performance tips from book "Thinking with Types"
  "
    Whenever your generic code needs to do something in f,
    it should use liftYoneda, and the final interface to your
    generic code should make a call to lowerYoneda to hide it
    as an implementation detail.
    This argument holds exactly when replacing Functor
    with Applicative or Monad, and Yoneda with Curried or
    Codensity respectively.
  " 
-}

-- >>>:t runYoneda
-- runYoneda :: Yoneda f a -> forall b. (a -> b) -> f b

-- >>>:t flip fmap
-- flip fmap :: Functor f => f a -> (a -> b) -> f b

-- >>>:kind! Yoneda [] Int 
-- Yoneda [] Int :: *
-- = Yoneda [] Int

-- >>>:t liftYoneda
-- liftYoneda :: Functor f => f a -> Yoneda f a
-- >>>:t liftYoneda [] 
-- liftYoneda [] :: Yoneda [] a

-- >>>:t lowerYoneda (liftYoneda [])
-- lowerYoneda (liftYoneda []) :: [a]

-- >>>:t runCodensity
-- runCodensity :: Codensity m a -> forall b. (a -> m b) -> m b


-- >>>:t (>>=)
-- (>>=) :: Monad m => m a -> (a -> m b) -> m b

-- >>>:t runCurried @(Yoneda _) @(Yoneda _)
-- runCurried @(Yoneda _) @(Yoneda _)
--   :: Curried (Yoneda w1) (Yoneda w2) a
--      -> forall r. Yoneda w1 (a -> r) -> Yoneda w2 r

-- >>>:t flip (<*>)
-- flip (<*>) :: Applicative f => f a -> f (a -> b) -> f b

-- >>>:k Curried
-- Curried :: (* -> *) -> (* -> *) -> * -> *

