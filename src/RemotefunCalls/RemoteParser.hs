{-# LANGUAGE RecordWildCards #-}
module RemotefunCalls.RemoteParser where


import Language.Haskell.Exts hiding (name)
import Language.Haskell.Meta.Syntax.Translate (toType)
import Language.Haskell.TH as TH
import Data.Char

data FuncInfo = FuncInfo 
      { name :: String
      , ty   :: TH.Type
      }

parseRemoteInterface :: String -> Q [FuncInfo]
parseRemoteInterface = undefined

  
genClientStubs = undefined