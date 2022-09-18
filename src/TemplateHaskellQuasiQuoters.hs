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
--{-# LANGUAGE TypeFamilies #-}
--{-# LANGUAGE TypeInType #-}
--{-# LANGUAGE UndecidableInstances #-}
--{-# LANGUAGE ScopedTypeVariables #-}
--{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE LambdaCase #-}

module TemplateHaskellQuasiQuoters where

import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax


-- | Oxford bracket we vew been using
-- are called quasiquoters and it is defined 
-- 

-- >>>:i QuasiQuoter
-- type QuasiQuoter :: *
-- data QuasiQuoter
--   = QuasiQuoter {quoteExp :: String -> Q Exp,
--                  quotePat :: String -> Q Pat,
--                  quoteType :: String -> Q Type,
--                  quoteDec :: String -> Q [Dec]}
--   	-- Defined in ‘Language.Haskell.TH.Quote’

str :: QuasiQuoter
str = QuasiQuoter 
    { quoteExp  = stringE
    , quotePat  = undefined
    , quoteType = undefined
    , quoteDec  = undefined
    }
