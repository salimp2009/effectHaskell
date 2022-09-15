--{-# LANGUAGE ConstraintKinds #-}
--{-# LANGUAGE EmptyCase #-}
--{-# LANGUAGE FlexibleInstances #-}
--{-# LANGUAGE GADTs #-}
--{-# LANGUAGE InstanceSigs #-}
--{-# LANGUAGE MultiParamTypeClasses #-}
--{-# LANGUAGE RankNTypes #-}
--{-# LANGUAGE ScopedTypeVariables #-}
--{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE TypeApplications #-}
--{-# LANGUAGE TypeFamilies #-}
--{-# LANGUAGE TypeInType #-}
--{-# LANGUAGE UndecidableInstances #-}
--{-# LANGUAGE ScopedTypeVariables #-}
--{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE StandaloneKindSignatures #-}
-- {-# LANGUAGE DerivingStrategies #-}

module TemplateHaskellPredicates where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

-- | geometric shapes to be use in example
data Shape = Circle Double
           | Square Double
           | Triangle Double Double Double
           
-- | typical predicates that might be needed        
-- isCircle   :: Shape -> Bool
-- isSquare   :: Shape -> Bool
-- isTriangle :: Shape -> Bool        

-- | goal is to generate those using TH
-- the plan is;
-- Reify given data-type name and get info for data constructors
-- Use TH to generate predicate definitions
-- Use TH splices to generate code for predicates

-- >>>:t reify
-- reify :: Name -> Q Info


           


