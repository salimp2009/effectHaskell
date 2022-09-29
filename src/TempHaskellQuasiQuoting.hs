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
compose = [| \l r x -> l(r x) |]

-- >>> $compose (*2) (+1) 0
-- 2

[d| class TupleX t r | t -> r where 
      _X :: t ->r 
  |]

-- >>> [d| class TupleX t r | t -> r where _X :: t ->r |]
-- [ClassD [] TupleX_0 [PlainTV t_2 (),PlainTV r_3 ()] [FunDep [t_2] [r_3]] 
--     [SigD _X_1 (AppT (AppT ArrowT (VarT t_2)) 


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
generateTupleClass :: Int -> Q [Dec]
generateTupleClass size = undefined
