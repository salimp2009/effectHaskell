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
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
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

-- >>>:i DecsQ
-- type DecsQ :: *
-- type DecsQ = Q [Dec]
--   	-- Defined in ‘Language.Haskell.TH.Lib.Internal’

precompute :: [Int] -> DecsQ
--precompute :: Monad m => [Int] -> m [Dec]
precompute xs = do 
      let name = mkName "lookupTable"
          patterns = map intToPat xs
          fnBodies = map precomputeInteger xs
          clauses =  precompClauses fnBodies patterns -- <> lastClause
      return [FunD name clauses ]
  where
    intToPat :: Int -> Pat
    intToPat = LitP . IntegerL . toInteger

    precomputeInteger :: Int -> Exp
    precomputeInteger = LitE . DoublePrimL . toRational . bigMathProblem

    precompClauses :: [Exp] -> [Pat] -> [Clause]
    precompClauses = zipWith (\body patrn -> Clause [patrn] (NormalB body) [])

    x' = mkName "x"
    lastClause = [Clause [VarP x'] (NormalB appBody) []]
    appBody = AppE (VarE (mkName "bigMathProblem")) (VarE  x')

bigMathProblem :: Int -> Double
bigMathProblem x = fromIntegral @Int @Double (x * x) + 100.0233
    
   
-- >>>:t Clause
-- Clause :: [Pat] -> Body -> [Dec] -> Clause

-- | there was an error so created 
-- version matching exact code in the book
-- to make sure that I dont make any mistake
-- both has the same error 
precompute2 xs = do
  let name = mkName "lookupTable2"
      patterns = map intToPat2 xs
      fnBodies = map precomputeInteger2 xs
      precomputedClauses =
        zipWith (\body ptr -> Clause [ptr] (NormalB body) []) fnBodies patterns
      x' = mkName "x"
      lastClause = [Clause [VarP x'] (NormalB appBody) []]
      appBody = AppE (VarE (mkName "bigMathProblem")) (VarE x')
      clauses = precomputedClauses  ++ lastClause
  return [FunD name clauses]

intToPat2 :: Int -> Pat
intToPat2 = LitP . IntegerL . toInteger

precomputeInteger2 :: Int -> Exp
precomputeInteger2 = LitE . DoublePrimL . toRational . bigMathProblem


-- >>>

-- >>>:i Body
-- type Body :: *
-- data Body = GuardedB [(Guard, Exp)] | NormalB Exp
--   	-- Defined in ‘Language.Haskell.TH.Syntax’

-- >>>:i NormalB
-- type Body :: *
-- data Body = ... | NormalB Exp
--   	-- Defined in ‘Language.Haskell.TH.Syntax’

-- >>>:i Exp
-- type Exp :: *
-- data Exp
--   = VarE Name
--   | ConE Name
--   | LitE Lit
--   | AppE Exp Exp
--   | AppTypeE Exp Type
--   | InfixE (Maybe Exp) Exp (Maybe Exp)
--   | UInfixE Exp Exp Exp
--   | ParensE Exp
--   | LamE [Pat] Exp
--   | LamCaseE [Match]
--   | TupE [Maybe Exp]
--   | UnboxedTupE [Maybe Exp]
--   | UnboxedSumE Exp SumAlt SumArity
--   | CondE Exp Exp Exp
--   | MultiIfE [(Guard, Exp)]
--   | LetE [Dec] Exp
--   | CaseE Exp [Match]
--   | DoE (Maybe ModName) [Stmt]
--   | MDoE (Maybe ModName) [Stmt]
--   | CompE [Stmt]
--   | ArithSeqE Range
--   | ListE [Exp]
--   | SigE Exp Type
--   | RecConE Name [FieldExp]
--   | RecUpdE Exp [FieldExp]
--   | StaticE Exp
--   | UnboundVarE Name
--   | LabelE String
--   | ImplicitParamVarE String
--   	-- Defined in ‘Language.Haskell.TH.Syntax’
-- instance Eq Exp -- Defined in ‘Language.Haskell.TH.Syntax’
-- instance Ord Exp -- Defined in ‘Language.Haskell.TH.Syntax’
-- instance Show Exp -- Defined in ‘Language.Haskell.TH.Syntax’
-- instance [safe] Ppr Exp -- Defined in ‘Language.Haskell.TH.Ppr’


-- >>>:i Pat
-- type Pat :: *
-- data Pat
--   = LitP Lit
--   | VarP Name
--   | TupP [Pat]
--   | UnboxedTupP [Pat]
--   | UnboxedSumP Pat SumAlt SumArity
--   | ConP Name [Pat]
--   | InfixP Pat Name Pat
--   | UInfixP Pat Name Pat
--   | ParensP Pat
--   | TildeP Pat
--   | BangP Pat
--   | AsP Name Pat
--   | WildP
--   | RecP Name [FieldPat]
--   | ListP [Pat]
--   | SigP Pat Type
--   | ViewP Exp Pat
--   	-- Defined in ‘Language.Haskell.TH.Syntax’
-- instance Eq Pat -- Defined in ‘Language.Haskell.TH.Syntax’
-- instance Ord Pat -- Defined in ‘Language.Haskell.TH.Syntax’
-- instance Show Pat -- Defined in ‘Language.Haskell.TH.Syntax’
-- instance [safe] Ppr Pat -- Defined in ‘Language.Haskell.TH.Ppr’






