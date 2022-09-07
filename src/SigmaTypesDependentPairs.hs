{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE UndecidableInstances #-}

module SigmaTypesDependentPairs where

import Data.Aeson
import Data.Constraint
import Data.Kind (Type)
import Data.Maybe (mapMaybe)
import Data.Ord.Singletons
import Data.Singletons.TH
import Data.String.Singletons
import Prelude.Singletons

{- 
  "Sigma types also known as dependent pairs, generalize
  arbitrarily-deeply nested Either types parameterized by
  a type. When viewed through the lens of the
  Curry–Howard isomorphism, they correspond to the
  existential quantifier ∃. ...
  ... Sigma types are the pair of an
  existential singleton and a type indexed by that singleton"
-}

data Sigma (f :: k -> Type) where
  Sigma :: Sing a -> f a -> Sigma f


withSigma :: (forall (a::k) . Sing a -> f a ->r) -> Sigma f -> r
withSigma c (Sigma sa f) = c sa f

-- | toSigma lifts an arbitrary f a into Sigma f
-- >>>:t toSigma
-- toSigma :: forall {k} {a :: k} {f :: k -> *}. SingI a => f a -> Sigma f
toSigma :: SingI a => f a -> Sigma f
toSigma fa  = Sigma sing fa


-- >>>:t fromSigma
-- fromSigma
--   :: forall {k} {a :: k} {f :: k -> *}.
--      (SingI a, SDecide k) =>
--      Sigma f -> Maybe (f a)
fromSigma :: forall k (a::k) (f :: k -> Type)
           . (SingI a, SDecide k)
          => Sigma f -> Maybe (f a) 
fromSigma (Sigma s f) =
  case s %~ sing @a of
    Proved Refl  -> Just f
    Disproved _  -> Nothing
