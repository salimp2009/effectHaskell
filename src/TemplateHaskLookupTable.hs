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
--{-# LANGUAGE StandaloneKindSignatures #-}
--{-# LANGUAGE LambdaCase #-}
-- {-# LANGUAGE DerivingStrategies #-}
module TemplateHaskLookupTable where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax


-- precompute :: [Int] -> DecsQ
-- precompute xs = do 
--       -- ..............
--       return [FunD Name [Clause] ]