{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE TypeApplications #-}
--{-# LANGUAGE TypeFamilies #-}
--{-# LANGUAGE TypeInType #-}
--{-# LANGUAGE UndecidableInstances #-}
--{-# LANGUAGE ScopedTypeVariables #-}
--{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module TempHaskellQuasiQuoting where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Control.Monad
import Data.Traversable (for)


-- >>>runQ [e|1 + 2 |]
-- InfixE (Just (LitE (IntegerL 1))) (VarE GHC.Num.+) (Just (LitE (IntegerL 2)))

-- >>>runQ [e| (1 + ) |]
-- InfixE (Just (LitE (IntegerL 1))) (VarE GHC.Num.+) Nothing

-- >>>$[e|1 + 2 |]
-- 3

-- >>>$([e| (1 + ) |]) 3
-- 4

-- >>>:t [e|1 + 2 |]
-- [e|1 + 2 |] :: Quote m => m Exp

-- >>>:t runQ
-- runQ :: Quasi m => Q a -> m a

-- >>>$([| 1+2 |])
-- 3

-- >>>:t [| 1+2 |]
-- [| 1+2 |] :: Quote m => m Exp

-- >>>runQ [| 1 + 2 |]
-- InfixE (Just (LitE (IntegerL 1))) (VarE GHC.Num.+) (Just (LitE (IntegerL 2)))

-- >>>runQ [| (4 , 5) |]
-- TupE [Just (LitE (IntegerL 4)),Just (LitE (IntegerL 5))]

-- >>>runQ [d| decl :: Int ; decl = 1 + 2 |]
-- [SigD decl_7 (ConT GHC.Types.Int),ValD (VarP decl_7) (NormalB (InfixE (Just (LitE (IntegerL 1))) (VarE GHC.Num.+) (Just (LitE (IntegerL 2))))) []]

[d| decl :: Int -> Int
    decl = (+) 1   
 |]

newVal :: Int -> Int
newVal = decl 

compose :: Q Exp
compose  = [| \l r x -> l(r x) |]
-- >>> $compose (*2) (+1) 0
-- 2


-- >>>:set -XFlexibleContexts
-- >>>$(compose 2)   (*2) (+1)
-- Couldn't match expected type: t0 -> ExpQ
--             with actual type: Q Exp


-- >>>:i TupleX 
-- type TupleX :: * -> * -> Constraint
-- class TupleX t r | t -> r where
--   _X :: t -> r
--   {-# MINIMAL _X #-}
--   	-- Defined at C:developerhaskelleffectiveHaskellbookchapter9MonadsmonadTypeClasssrcTempHaskellQuasiQuoting.hs:64:1


-- >>> [| (1, 2, 3) |]
-- TupE [Just (LitE (IntegerL 1)),Just (LitE (IntegerL 2)),Just (LitE (IntegerL 3))]

-- >>>[d| x :: (a , b , c) ; x = undefined |]
-- [SigD x_7 
--  (AppT (AppT (AppT (TupleT 3) (VarT a_4)) (VarT b_5)) (VarT c_6))
--   , ValD (VarP x_7) (NormalB (VarE GHC.Err.undefined)) []]

-- >>>runQ [d| x:: a -> b; x = undefined |]
-- [SigD x_2 
--  (AppT (AppT ArrowT (VarT a_0)) (VarT b_1))
-- ,ValD (VarP x_2) (NormalB (VarE GHC.Err.undefined)) []]

-- >>>[d| x :: (a , b , c) ->d -> e; x = undefined |]
-- [SigD x_8 
--  (AppT 
--    (AppT ArrowT (AppT (AppT (AppT (TupleT 3) (VarT a_3)) (VarT b_4)) (VarT c_5))) 
--    (AppT (AppT ArrowT (VarT d_6)) (VarT e_7))
--  )
-- ,ValD (VarP x_8) (NormalB (VarE GHC.Err.undefined)) []]

[d| class TupleX t r | t -> r where 
      _X :: t ->r 
  |]

-- >>> [d| class TupleX t r | t -> r where _X :: t ->r |]
-- [ClassD [] TupleX_0 [PlainTV t_2 (),PlainTV r_3 ()] [FunDep [t_2] [r_3]] 
--     [SigD _X_1 (AppT (AppT ArrowT (VarT t_2))] 
-- | ^ this is what we are doing X being any given size of a Tuple
-- t refern to the nth element and r is the nth element of the given Tuple
generateTupleClass :: Int -> Q [Dec]
generateTupleClass size = do
  unless (size > 0) $ 
    fail $ "Non-positive size: " <> size'
  pure [cDecl]  
    where 
      size' = show size
      className = mkName ("Tuple" <> size')
      methodName = mkName ('_' : size')
      t = mkName "t"
      r = mkName "r"
      cDecl = ClassD [] className [PlainTV t (),PlainTV r ()] [FunDep [t] [r]] [mDecl]
      mDecl = SigD methodName (AppT (AppT ArrowT (VarT t)) (VarT r))


-- >>>runQ [d| instance TupleX (a, b, c) c where _X (_, _, c) = c |]      
-- [InstanceD Nothing [] 
--  (AppT (AppT (ConT TempHaskellQuasiQuoting.TupleX) 
--    (AppT (AppT (AppT (TupleT 3) (VarT a_4)) (VarT b_5)) (VarT c_6))) (VarT c_6)) 
--    [FunD TempHaskellQuasiQuoting._X [Clause [TupP [WildP,WildP,VarP c_7]] (NormalB (VarE c_7)) []]]]
-- | need to create instance of the TupleX class

generateTupleInstance :: Int -> Int -> Q [Dec]
generateTupleInstance element size = do 
  unless (size > 0) $ 
    fail $ "Non-positive size: " <> size'
  unless (size >= element) $ 
    fail $ "Can't extract element: " <> element' <> " off " <> size'  
  pure [iDecl]
  where
    element' = show element
    size'    = show size 
    className   = mkName ("Tuple" <> element')
    methodName  = mkName ('_' : element')
    x =  mkName "x"  -- << this is the name for requested element
    vars = [mkName ('t' : show n) | n <- [1..size]] -- << these are the other variable not including requested one
    signature = foldl (\acc var -> AppT acc (VarT var)) (TupleT size) vars  -- << this is the part ->; (AppT (AppT (AppT (TupleT 3) (VarT a_4)) ...) (VarT c_6))
    iDecl =  InstanceD Nothing [] (AppT (AppT (ConT className) signature) (VarT $ mkName ('t' : element'))) [mDecl]
    mDecl = FunD  methodName [Clause [TupP $ replicate (element -1) WildP  <> [VarP x] <> replicate (size - element) WildP] (NormalB $ VarE x) []]   

generateTupleBoilerPlate :: Int -> Q [Dec]
generateTupleBoilerPlate size =
  concatFor [1..size] $ \classDecIndex -> do
    cDecl   <- generateTupleClass classDecIndex                   -- << cDecl  :: [Dec]
    iDecls  <- for [1..classDecIndex] $ \instanceDecIndex ->      -- << iDecls :: [[Dec]]
        generateTupleInstance instanceDecIndex classDecIndex
    pure $ concat (cDecl : iDecls)                                -- pure _ :: [Dec] -> Q [Dec]
  where
    concatFor :: [Int] -> (Int -> Q [Dec]) -> Q [Dec]
    concatFor xs = fmap concat . for xs       



-- >>>:t ClassD      
-- ClassD :: Cxt -> Name -> [TyVarBndr ()] -> [FunDep] -> [Dec] -> Dec

-- >>>:i InstanceD
-- type Dec :: *
-- data Dec = ... | InstanceD (Maybe Overlap) Cxt Type [Dec] | ...
--   	-- Defined in ‘Language.Haskell.TH.Syntax’

-- >>>:t SigD
-- ValD :: Pat -> Body -> [Dec] -> Dec

-- >>>:t FunDep
-- FunDep :: [Name] -> [Name] -> FunDep

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

-- >>>:i Dec
-- type Dec :: *
-- data Dec
--   = FunD Name [Clause]
--   | ValD Pat Body [Dec]
--   | DataD Cxt Name [TyVarBndr ()] (Maybe Kind) [Con] [DerivClause]
--   | NewtypeD Cxt Name [TyVarBndr ()] (Maybe Kind) Con [DerivClause]
--   | TySynD Name [TyVarBndr ()] Type
--   | ClassD Cxt Name [TyVarBndr ()] [FunDep] [Dec]
--   | InstanceD (Maybe Overlap) Cxt Type [Dec]
--   | SigD Name Type
--   | KiSigD Name Kind
--   | ForeignD Foreign
--   | InfixD Fixity Name
--   | PragmaD Pragma
--   | DataFamilyD Name [TyVarBndr ()] (Maybe Kind)
--   | DataInstD Cxt
--               (Maybe [TyVarBndr ()])
--               Type
--               (Maybe Kind)
--               [Con]
--               [DerivClause]
--   | NewtypeInstD Cxt
--                  (Maybe [TyVarBndr ()])
--                  Type
--                  (Maybe Kind)
--                  Con
--                  [DerivClause]
--   | TySynInstD TySynEqn
--   | OpenTypeFamilyD TypeFamilyHead
--   | ClosedTypeFamilyD TypeFamilyHead [TySynEqn]
--   | RoleAnnotD Name [Role]
--   | StandaloneDerivD (Maybe DerivStrategy) Cxt Type
--   | DefaultSigD Name Type
--   | PatSynD Name PatSynArgs PatSynDir Pat
--   | PatSynSigD Name PatSynType
--   | ImplicitParamBindD String Exp
--   	-- Defined in ‘Language.Haskell.TH.Syntax’

