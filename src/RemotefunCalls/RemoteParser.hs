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
parseRemoteInterface quote = concat <$> mapM (funcInfo . parseDecl) funcs
      where
            funcs = filter (not . null) $ map (dropWhile isSpace) $ lines quote

funcInfo :: ParseResult (Decl SrcSpanInfo) -> Q [FuncInfo]             
funcInfo (ParseOk (TypeSig _ ids t)) = 
      pure $ [FuncInfo {..} |  let ty = toType t,  
                               Ident _ name <- ids]

funcInfo err = fail $ " " <> show err                       