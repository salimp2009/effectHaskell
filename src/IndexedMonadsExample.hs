{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
--{-# LANGUAGE RebindableSyntax #-}

module IndexedMonadsExample where

import Control.Monad.Indexed
import Data.Coerce
-- import Language.Haskell.DoNotation
-- import Prelude hiding (Monad (..), pure)

{-
    "Indexed monads are a generalization of monads that
  allow us to enforce pre- and post-conditions on monadic
  actions. They’re a great tool for describing protocols and
  contracts at the type-level."
-}

-- | will implement a monad that tracks files that are open
-- and requires them to be closed exactly.
-- Failure to requirements will cause program not to compile
-- Will looks at how staticaly enforce resource allocation linearly


-- >>>:k IxMonad
-- IxMonad :: (k -> k -> * -> *) -> Constraint

-- >>>:t ibind
-- ibind
--   :: forall {k} {m :: k -> k -> * -> *} {a} {j :: k} {k1 :: k} {b}
--             {i :: k}.
--      IxMonad m =>
--      (a -> m j k1 b) -> m i j a -> m i k1 b

-- | definition IxMonad
-- class IxApplicative m => IxMonad m where
--    ibind :: (a -> m j k b) -> m i j a -> m i k b
{- 
    "ibind works by matching up the post-condition of an
  action m i j with the precondition of another m j k. In
  doing so, the intermediary condition j is eliminated,
  giving us the precondition from the first action and the
  post-condition from the second (m i k)."
-}

newtype Ix m i j a = Ix 
  { unsafeRunIx :: m a}
  deriving (Functor, Applicative, Monad)

instance Functor m => IxFunctor (Ix m) where
  imap = fmap

instance Applicative m => IxPointed (Ix m) where  
  ireturn = pure  

instance Applicative m => IxApplicative (Ix m) where
  iap :: forall i j k a b . Ix m i j (a -> b) -> Ix m j k a -> Ix m i k b
  iap = coerce $ (<*>) @m @a @b

instance Monad m => IxMonad (Ix m) where
  -- ibind :: (a -> m j k b) -> m i j a -> m i k b
  ibind :: forall i j k a b 
          . (a -> Ix m j k b) 
        ->  Ix m i j a 
        ->  Ix m i k b
  ibind = coerce $ (=<<) @m @a @b        

       
