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
import Control.Monad (replicateM)
import Control.Monad.Reader (liftIO)

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
  info  <- reify fn
  arity <- getArity info
  varNames <- replicateM (arity -1) (newName "arg")
  b <- newName "b"  
  let fnName    = mkName . nameBase $ fn
      bound     = AppE (VarE '(>>=)) (VarE reader) 
      binder    = AppE bound . LamE [VarP b]        -- << similar to; (>>=) reader (\b -> ...)
      varExprs   = map VarE (b : varNames)            
      fullExprs  = foldl AppE (VarE fn) varExprs
      liftedExpr = AppE (VarE 'liftIO) fullExprs
      final      = binder liftedExpr
      varPat     = map VarP varNames
  return (FunD fnName [Clause varPat (NormalB final) []])
    where
      getArity info'= maybe (reportError "Unable to get arity of name" >> return 0 )
                       (return . functionLevels) 
                       (getType info')


deriveReader :: Name -> DecsQ
deriveReader rd =
              mapM (decForFunc rd)
              [ 'destroyUserBackend
              , 'housekeepBackend
              , 'getUserIdByName
              , 'getUserById
              , 'listUsers
              , 'countUsers
               , 'createUser
               , 'updateUser
               , 'updateUserDetails
               , 'authUser
               , 'deleteUser
               ]  
-- | these function declarations and backend functions needs to be implemented
-- in another file
-- and needs to be called as ; 
-- deriverReader 'backend  
-- but that will be out of the example scope   

backend = undefined
destroyUserBackend = undefined 
housekeepBackend = undefined
getUserIdByName = undefined
getUserById = undefined
listUsers = undefined
countUsers = undefined
createUser = undefined
updateUser = undefined
updateUserDetails = undefined
authUser = undefined
deleteUser = undefined
          
{- 
  fn        ~ VarE fn
  fn a      ~ AppE (VarE fn) (VarE a)
  fn a b    ~ AppE (AppE (VarEfn) (VarE a)) (VarE b)
  fn a b c  ~ AppE (AppE (AppE (VarEfn) (VarE a)) (VarE b)) (VarE c)
-}

-- >>>:i AppE
-- type Exp :: *
-- data Exp = ... | AppE Exp Exp | ...
--   	-- Defined in ‘Language.Haskell.TH.Syntax’

-- >>>:i VarE
-- type Exp :: *
-- data Exp = VarE Name | ...
--   	-- Defined in ‘Language.Haskell.TH.Syntax’

-- >>>:i LamE
-- type Exp :: *
-- data Exp = ... | LamE [Pat] Exp | ...
--   	-- Defined in ‘Language.Haskell.TH.Syntax’

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

-- | this is the boilerplate  to create several functions
-- that we are trying automate using TH
{- 
derivedFunction arg1 arg2 ... argn =
  ((>>=) backend)
    (\b -> liftIO ((...(((function b) arg1) arg2)...) argn))
-}

-- >>>:i newName
-- type Quote :: (* -> *) -> Constraint
-- class Monad m => Quote m where
--   newName :: String -> m Name
--   	-- Defined in ‘Language.Haskell.TH.Syntax’

-- >>>:i mkName
-- mkName :: String -> Name
--   	-- Defined in ‘Language.Haskell.TH.Syntax’
