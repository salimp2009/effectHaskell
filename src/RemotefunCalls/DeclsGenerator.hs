{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}

module RemotefunCalls.DeclsGenerator where

import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Network.Socket (PortNumber)

import RemotefunCalls.RemoteParser

remote :: QuasiQuoter
remote = QuasiQuoter 
        { quoteExp = undefined
        , quotePat = undefined
        , quoteType = undefined
        , quoteDec = quoteFuncInfoDec
        }

quoteFuncInfoDec :: String -> Q [Dec]
quoteFuncInfoDec quote = parseRemoteInterface quote >>= genClientStubs

genClientStubs :: [FuncInfo] -> Q [Dec]
genClientStubs fis = concat <$> mapM (genClientStub "callRemote") fis

-- >>>:t funD
-- funD :: Quote m => Name -> [m Clause] -> m Dec

-- >>>:t clause
-- clause :: Quote m => [m Pat] -> m Body -> [m Dec] -> m Clause

-- >>>:t Clause
-- Clause :: [Pat] -> Body -> [Dec] -> Clause

-- >>>:t normalB
-- normalB :: Quote m => m Exp -> m Body

-- >>>:t NormalB
-- NormalB :: Exp -> Body

-- >>>:t FunD
-- FunD :: Name -> [Clause] -> Dec
genClientStub :: String -> FuncInfo -> Q [Dec]
genClientStub callee FuncInfo{..} =  do
       funcImpl <- funD funName [clause [] (normalB stubBody) []]
       pure [typeSig, funcImpl]
     where
        funName  = mkName name
        typeSig  = SigD funName ty
        stubBody = [| $(curryAll (arity ty)) $ $(dyn callee) name|]  
 
-- >>>:t dyn       
-- dyn :: Quote m => String -> m Exp

-- | arity inspect TH type type representation 
-- determines the number of function arguments
-- However this function does not all typea (about 23-24) in TH.Type 
arity :: Type -> Int
arity (ForallT _ _ rest) = arity rest
arity (AppT (AppT ArrowT _) rest) = arity rest + 1
arity _ = 0

                        



-- >>>:t curry
-- curry :: ((a, b) -> c) -> a -> b -> c

-- >>>:t $(curryAll 3)
-- $(curryAll 3) :: (((a, b1), b2) -> c) -> a -> b1 -> b2 -> c

-- >>>:t $(curryAll 1)
-- $(curryAll 1) :: a -> a

-- >>>:t $([| id |])
-- $([| id |]) :: a -> a

-- | creates a lamda function which creates a function with many argument 
-- from a function with one tupple argument
-- single argument function is the part that runs over the network
-- user only deals with the uncurried version
curryAll :: Int -> Q Exp
curryAll 0 = [| \f -> f () |] 
curryAll 1 = [| id |]
curryAll n
          | n > 1 = [| curry. $(curryAll (n-1)) |]
          | otherwise = fail "curryAll argument can't be"

