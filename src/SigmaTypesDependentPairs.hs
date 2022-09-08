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
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE StandaloneKindSignatures #-}



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
-- | casting a Sigma f back to f a
{- 
  "By pattern matching on Refl at (case expression) , GHC learns 
  that a ~ t, where t is the “existential” type inside of the Sigma.
  With this equality in hand, it’s clearly safe to return the 
  f t when asking for f a.
"

-}
fromSigma :: forall k (a::k) (f :: k -> Type)
           . (SingI a, SDecide k)
          => Sigma f -> Maybe (f a)
fromSigma (Sigma s f) =
  case s %~ sing @a of
    Proved Refl  -> Just f
    Disproved _  -> Nothing


-- >>>:t dict1
-- dict1
--   :: forall {k} {c :: * -> Constraint} {f :: k -> *} {a :: k}.
--      Dict1 c f =>
--      Sing a -> Dict (c (f a))

-- >>>:k Dict1
-- Dict1 :: (* -> Constraint) -> (k -> *) -> Constraint
-- >>>:k Dict1 Eq Maybe
-- Dict1 Eq Maybe :: Constraint

-- | dict fnuction can be generalized into a type class to 
-- provide total constraints on a given singleton
-- c is Constraint kind ; Type -> Constraint
-- f is K -> Type
-- since Sing has a kind K the f will the type that Constraint needs


type Dict1 :: forall k. (Type -> Constraint) -> (k -> Type) -> Constraint
class Dict1  c f where
  dict1 :: Sing a -> Dict (c (f a))

instance (Dict1 Eq (f :: k ->Type)
         , SDecide k) 
         => Eq (Sigma f) where
  Sigma sa fa == Sigma sb fb =
      case sa %~ sb of
        Proved Refl ->
          case dict1 @_ @Eq @f sa of
            Dict -> fa  == fb
        Disproved _  -> False

instance (Dict1 Show (f::k ->Type)
         , Show (Demote k )
         , SingKind k) => Show (Sigma f) where
  show (Sigma sa fa) = 
    case dict1 @_ @Show @f sa of
      Dict -> mconcat 
            ["Sigma"
            , show $ fromSing sa
            , " ("
            , show fa
            , ")"
            ]

instance ( Dict1 Ord(f ::k -> Type)
         , Dict1 Eq f
         , SDecide k  
         , SingKind k
        , Ord (Demote k) )            
        => Ord (Sigma f) where
  compare (Sigma sa fa) (Sigma sb fb) =
    case sa %~ sb of 
      Proved Refl -> 
          case dict1 @_ @Ord @f sa of
            Dict -> compare fa fb
      Disproved _ -> 
            compare (fromSing sa) (fromSing sb)