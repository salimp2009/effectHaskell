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
module TypedTemplateHaskell where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

-- >>>:i TExp
-- type role TExp nominal
-- type TExp :: * -> *
-- newtype TExp a = TExp {unType :: Exp}
--   	-- Defined in ‘Language.Haskell.TH.Syntax’

{- 
  type role TExp nominal
  newtype TExp (a :: TYPE (r :: RuntimeRep)) = TExp
    { unType :: Exp }
-}

-- | regular TH does not do type checking
-- >>>runQ [| 42::String |]
-- SigE (LitE (IntegerL 42)) (ConT GHC.Base.String)

-- typed TH does type checking
-- >>>runQ [|| 42::String ||]
-- Couldn't match expected type: Q a
--             with actual type: Code m0 String

mytypeNum :: Double
mytypeNum = $$([|| 42::Double ||]) 

-- >>>:t [|| 42::Double ||]
-- [|| 42::Double ||] :: Quote m => Code m Double

-- >>>mytypeNum
-- 42.0 

-- >>>:set -fprint-explicit-foralls 
-- >>>:set  -fprint-explicit-kinds
-- >>>:i ($)
-- ($) :: forall a b. (a -> b) -> a -> b 	-- Defined in ‘GHC.Base’
-- infixr 0 $

