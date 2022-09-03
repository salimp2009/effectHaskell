{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module AdHOCImplemntSingleton where

import Control.Monad.Trans.Writer
import Data.Constraint (Dict (..))
import Data.Foldable (for_)
import Data.Kind (Type)


data SBool (b::Bool) where
  STrue  :: SBool 'True
  SFalse :: SBool 'False

-- >>>fromBool STrue
-- True

-- >>>fromBool SFalse
-- False
fromBool :: SBool b -> Bool
fromBool STrue  = True
fromBool SFalse = False

data SomeSBool where
  SomeSBool :: SBool b -> SomeSBool

withSomeBool :: SomeSBool -> (forall (b::Bool). SBool b -> r) -> r
withSomeBool (SomeSBool sb)  f = f sb


