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
module OpenProducts where

import Data.Kind (Constraint, Type)
import Data.Proxy (Proxy (..))
import qualified Data.Vector as V
import Fcf
import GHC.OverloadedLabels (IsLabel (..))
import GHC.TypeLits 
import Unsafe.Coerce (unsafeCoerce)

 -- | a container Any that will
-- existentialize away its k index.
data Anyc (f::k -> Type) where
  Anyc :: f t -> Anyc f

{-
  "This implementation of OpenProduct will to optimize
  for O(1) reads, and O(n) writes, although other trade-offs
  are possible. We thus define OpenProduct as a Data.Vector
  of Anys."
-}  

-- | ts is now keeps track of which type store in Vector Anyc
-- and it also associates them with names as Symbols so user can provide
-- names for the contents of the product
data OpenProduct (f::k -> Type) (ts :: [(k, Symbol)]) where
  OpenProduct :: V.Vector (Anyc f) -> OpenProduct f ts

nil :: OpenProduct f '[]  
nil = OpenProduct V.empty

{- 
  "Because all data inside an OpenProduct will be labeled by
  a SYMBOL, we need a way for users to talk about SYMBOLs at
  the term-level."
-}
data Key (key :: Symbol) = Key