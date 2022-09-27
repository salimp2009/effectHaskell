--{-# LANGUAGE ConstraintKinds #-}
--{-# LANGUAGE FlexibleInstances #-}
--{-# LANGUAGE GADTs #-}
--{-# LANGUAGE InstanceSigs #-}
--{-# LANGUAGE RankNTypes #-}
--{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
--{-# LANGUAGE TypeFamilies #-}
--{-# LANGUAGE TypeInType #-}
--{-# LANGUAGE UndecidableInstances #-}
--{-# LANGUAGE DataKinds           #-}
--{-# LANGUAGE StandaloneKindSignatures #-}
--{-# LANGUAGE LambdaCase #-}
-- {-# LANGUAGE DerivingStrategies #-}
module TempHaskellMultiFuncDec where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

{- 
  fmap :: (a -> b) -> f a -> f b
  ~ (->) ((-> a b)) ((->) (f a) (f b)) 
  ~ (ArrowT) ((ArrowT) a b)) (ArrowT (f a) (f b))
  ~ (ArrowT ((ApplyT ArrowT a) b)) (ApplyT(ApplyT ArrowT (Apply f a) (Apply f b))
  ~ ApplyT (ArrowT (ApplyT (ApplyT ArrowT a) b)) (ApplyT(ApplyT ArrowT (ApplyT f a)) (ApplyT f b))
-}

-- >>>$(stringE . show =<< reify 'id)    
-- "VarI GHC.Base.id 
--    (ForallT 
--      [KindedTV a_6989586621679160728 SpecifiedSpec StarT] [] 
--      (AppT 
--        (AppT ArrowT (VarT a_6989586621679160728)) 
--        (VarT a_6989586621679160728)
--      )
--    )
--    Nothing"


-- | final goal is create a bunch functions using type signatures and TH
-- we need to get the arguments from types 
-- we will pattern match types until we get to arguments and
-- if the pattern match does match we call the function recursively 
-- and increase our counter
-- functionLevels ~ Arity
functionLevels :: Type -> Int
functionLevels = go 0
  where
    go :: Int -> Type -> Int
    go n (AppT (AppT ArrowT _) rest)  = go (n+1) rest
    go n (ForallT _ _ rest) = go n rest
    go n _  = n   

-- | when we querry a function we get Info   
-- a sum type that has various Constructor
-- we  pattern match to get the ones with Type to generator   
getType :: Info -> Maybe Type
getType (ClassOpI _ t   _) = Just t
getType (DataConI _ t  _)  = Just t
getType (VarI _ t _)       = Just t
getType (TyVarI _ t)       = Just t
getType _                  = Nothing

decForFunc :: Name -> Name -> Q Dec
decForFunc reader fn = do
  info <- reify fn
  arity <- maybe (reportError "Unable to get arity of name" >> return 0 )
                 (return . functionLevels) 
                 (getType info)
  return (FunD fnName [Clause varPat (NormalB final) []])

fnName = undefined
varPat = undefined 
final = undefined  