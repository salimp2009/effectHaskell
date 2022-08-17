{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module CustomTypeErrors where

import Data.Kind (Constraint, Type)
import Data.Proxy (Proxy (..))
import qualified Data.Vector as V
import Fcf
import GHC.OverloadedLabels (IsLabel (..))
import GHC.TypeLits 
import Unsafe.Coerce (unsafeCoerce)

import OpenProducts

-- type family TypeError (a :: ErrorMessage) :: b where

-- >>>1 True
-- Attempting to use a type as a function
-- in the type `Bool -> t`
-- Maybe you forgot to specify a function?
instance (TypeError 
            (Text "Attempting to use a type as a function"
             :$$: Text "in the type `"
             :<>: ShowType (a -> b)
             :<>: Text "`"
             :$$: Text "Maybe you forgot to specify a function?"
             )
      ) => Num (a -> b) where
  (*) = undefined
  (+) = undefined
  abs = undefined
  signum = undefined
  fromInteger = undefined
  negate = undefined
