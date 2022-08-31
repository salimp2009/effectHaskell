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
