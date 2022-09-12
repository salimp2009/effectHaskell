{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE DerivingStrategies #-}

module TempHaskellProjector where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

-- the goal is to be able to access 
-- elements of a tuple with more than 2 element
-- e.g 17 element such as;
-- 
{- 
  proj 17 _0 (x0,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) = x0
  ...
  ...
  ...
  proj 17 _16 (_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x16) = x16
-}

-- >>>runQ [| \(x, _, _) -> x |]
-- LamE [TupP [VarP x_0,WildP,WildP]] (VarE x_0)

-- >>>:t [| \(x, _, _) -> x |]
-- [| \(x, _, _) -> x |] :: Quote m => m Exp
proj :: Int -> Int -> Q Exp
proj n k = do
  x <- newName "x"
  let makePat j
        | j == k = VarP x
        | otherwise = WildP
  pure $ LamE [TupP $ map makePat [0..n-1]] (VarE x)




--- >>>$(proj 3 2) ("aaa", "bbb", "ccc")  
-- "ccc"

-- >>>:t $(proj 3 2) 
-- $(proj 3 2) :: (a, b, c) -> c

-- >>>:t proj
-- proj :: Int -> Int -> Q Exp


