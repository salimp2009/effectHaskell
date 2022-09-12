{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE DerivingStrategies #-}

module TempHaskellProjector where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

-- the goal is to be able to access 
-- elements of a tuple with more than 2 element
-- e.g 17 element such as;
-- 
{- 
  proj 17 _0 (x0,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) = x0
  ...
  ...
  ...
  proj 17 _16 (_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x16) = x16
-}

-- >>>runQ [| \(x, _, _) -> x |]
-- LamE [TupP [VarP x_0,WildP,WildP]] (VarE x_0)
proj :: Int -> Int -> Q Exp
proj = undefined
