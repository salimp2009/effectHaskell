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

-- >>>:t [| \(x, _, _) -> x |]
-- [| \(x, _, _) -> x |] :: Quote m => m Exp
proj :: Int -> Int -> Q Exp
proj n k = do 
  x <- newName "x"
  let makePat j
        | j == k = VarP x
        | otherwise = WildP
  pure $ LamE [TupP $ map makePat [0..n-1]] (VarE x)


--- >>>$(proj 3 2) ("aaa", "bbb", "ccc")  
-- "ccc"

-- >>>:t $(proj 3 2) 
-- $(proj 3 2) :: (a, b, c) -> c

-- >>>:t proj
-- proj :: Int -> Int -> Q Exp

-- >>>$(proj 3 4) ("aaa", "bbb", "ccc") 
-- The exact Name ‘x_aItl’ is not in scope
--   Probable cause: you used a unique Template Haskell name (NameU), 
--   perhaps via newName, but did not bind it
--   If that's it, then -ddump-splices might be useful

-- | need to revise the code to check requested index is not 
-- greater than the total num of elems

-- >>>$(proj2 3 2) ("aaa", "bbb", "ccc")
-- "ccc"

-- >>>$(proj2 3 3) ("aaa", "bbb", "ccc")
-- Incorrect projection3 of 3 elements 
proj2 :: Int -> Int -> Q Exp
proj2 n k  
  | n > 1 && 0 <=k && k < n = do
          x <- newName "x"
          let makePat j
                | j == k = VarP x
                | otherwise = WildP
          pure $ LamE [TupP $ map makePat [0..n-1]] (VarE x)
  | n <=1 = fail "Number of tuples elements must be > 1" 
  | otherwise = fail $ "Incorrect projection" 
                <> show k <> " of " <> show n <>" elements "


-- >>>$(proj3 3 2) ("aaa", "bbb", "ccc")
-- "ccc"

-- >>>$(proj3 3 3) ("aaa", "bbb", "ccc")
-- Incorrect projection3 of 3 elements 

-- | need to refactor code to delegate parts of it
-- to GHC with quotes and splices
-- TH allows nesting splices and quotes
-- using varE, varP, wildP, tupP functions instead of VarE, VarP, WildP, TupP.
-- these functions returns a Q monad 
-- they work better with the $(…) splice operator which
-- expects Q a values inside it
proj3 :: Int -> Int -> Q Exp
proj3 n k  
  | n > 1 && 0 <=k && k < n = do
          x <- newName "x"
          [| \ $(mkArg x) -> $(varE x)|]
  | n <=1 = fail "Number of tuples elements must be > 1" 
  | otherwise = fail $ "Incorrect projection" 
                <> show k <> " of " <> show n <>" elements "
  where
    mkPat x j 
          | j == k = varP x
          | otherwise = wildP
    mkArg x = tupP $ map (mkPat x ) [0..n-1]  
    
-- >>>:t TupP    
-- TupP :: [Pat] -> Pat
-- >>>:t tupP
-- tupP :: Quote m => [m Pat] -> m Pat

-- >>>:t (mkProjDec 2 1)
-- (mkProjDec 2 1) :: Q [Dec]

-- >>>runQ $ mkProjDec 2 1
-- [ValD (VarP proj_2_1) (NormalB (LamE [TupP [WildP,VarP x_3]] (VarE x_3))) []]

-- | Generating Declarations via TH
-- goal is generate all projector for a given number of 
  -- elements in a tuple 
mkProjName :: Int -> Int -> Name
mkProjName n k = mkName $ "proj_" <> show n <> "_" <> show k

mkProjDec :: Int -> Int -> Q [Dec]
mkProjDec n k = [d| $nm = $(proj3 n k)|] 
  where 
    nm = varP $ mkProjName n k

-- >>>runQ [t| forall a b. (a, b) -> b|]
-- ForallT [PlainTV a_2 SpecifiedSpec,PlainTV b_3 SpecifiedSpec] [] (AppT (AppT ArrowT (AppT (AppT (TupleT 2) (VarT a_2)) (VarT b_3))) (VarT b_3))
-- | goal is to create function top level declaration
-- need to use Typed Haskell to make it simpler
mkProjType :: Int -> Int -> Q Dec    
mkProjType n k = sigD nm funTy
  where 
    nm = mkProjName n k
    funTy = undefined   -- <<< to be implemented
-- >>>$$(liftTyped 5)
-- 5

-- >>>add1 x = [|| x + 1 ||]
-- >>>$$(add1 2)
-- 3

-- >>>msnd (x, y) = [||(forall x y. (x, y) -> y) ||]
-- >>>$$(msnd (1,2))
-- View pattern in expression context: forall x y . (x, y) -> y



-- >>>:k ForallT
-- ForallT :: [TyVarBndr Specificity] -> Cxt -> Type -> Type

-- >>>:k ValD    
-- ValD :: Pat -> Body -> [Dec] -> Dec

-- >>>:k VarP
-- VarP :: Name -> Pat

-- >>>:k NormalB
-- NormalB :: Exp -> Body

-- >>>:k LamE
-- LamE :: [Pat] -> Exp -> Exp

-- >>>$fpi
-- 6.283185307179586

-- >>>runQ fpi
-- InfixE (Just (VarE GHC.Float.pi)) (VarE GHC.Num.+) (Just (VarE pi))

-- >>>pi
-- 3.141592653589793

-- >>>gnew = let pi = 3 in $fpi
-- >>>gnew
-- 6.141592653589793

fpi :: Q Exp
fpi = [| pi + $(varE (mkName "pi")) |]



