{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
--{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE StandaloneKindSignatures #-}
--{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE EmptyCase #-}
--{-# LANGUAGE FlexibleInstances #-}


module SingletonsDependentPairs where

import Data.Singletons.TH
import Prelude.Singletons
import Data.Ord.Singletons

-- | Singletons package provides template Haskell
-- create singleton and automatically promotes term-level function
-- Below quasiquoter from TH [d| ... |] (aka Oxford Brackets) creates a declaration
-- and the function will create all definitions for Sing, SingKind, SingI and also 
-- [d| ... |], where the “…” is a list of top-level declarations;  
-- the quotation has type Quote m => m [Dec]
-- singletons will also generate SDecide instances if Eq definitions exist for Sing.. 

-- >>>:t singletons
-- singletons :: OptionsMonad q => q [Dec] -> q [Dec]
singletons [d|
  data TimeOfDay
    = Morning
    | Afternoon
    | Evening
    | Night
      deriving (Eq, Ord, Show)
  |]
