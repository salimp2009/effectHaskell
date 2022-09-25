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
--{-# LANGUAGE StandaloneKindSignatures #-}
--{-# LANGUAGE LambdaCase #-}
-- {-# LANGUAGE DerivingStrategies #-}
module TemplateHaskLookupTable where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import GHC.Float (int2Double)

{- 
  The goal is as described in "Production Haskell" book;
  to pass in a list of common numbers, 
  run the function on each to precompute them, and
  if we didn’t cache the number, have
  a clause that passes the value to the math function 
-}

precompute :: [Int] -> DecsQ
precompute xs = do 
      let name = mkName "lookupTable"
          patterns = map intToPat xs
          fnBodies = map precomputeInteger xs
          clauses =  precompClauses fnBodies patterns

      return [FunD name clauses ]
  where
    intToPat :: Int -> Pat
    intToPat = LitP . IntegerL . toInteger

    precomputeInteger :: Int -> Exp
    precomputeInteger = LitE . DoublePrimL . toRational . bigMathProblem

    precompClauses :: [Exp] -> [Pat] -> [Clause]
    precompClauses = zipWith (\body patrn -> Clause [patrn] (NormalB body) [])
      
    
bigMathProblem :: Int -> Double  
bigMathProblem x = int2Double (x+100) * 10.12345
