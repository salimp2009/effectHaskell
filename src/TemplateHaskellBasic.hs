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
-- {-# LANGUAGE StandaloneKindSignatures #-}
-- {-# LANGUAGE DerivingStrategies #-}

module TemplateHaskellBasic where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

-- >>>$expTH
-- 3

-- >>>:t expTH
-- expTH :: Q Exp

-- >>>$expTH
-- 3

-- >>>runQ expTH
-- InfixE (Just (LitE (IntegerL 1))) (VarE GHC.Num.+) (Just (LitE (IntegerL 2)))

-- >>>:t runQ
-- runQ :: Quasi m => Q a -> m a

-- >>>:t (runQ expTH)
-- >>>:t runQ
-- runQ :: Quasi m => Q a -> m a



-- >>>:kind! Quasi
-- Quasi :: (* -> *) -> Constraint
-- = Quasi

-- >>>:kind! Exp
-- Exp :: *
-- = Exp

-- >>>:t [| 1 + 2 |]
-- [| 1 + 2 |] :: Quote m => m Exp

-- >>>show (expTH)
-- No instance for (Show (Q Exp)) arising from a use of ‘show’
expTH :: Q Exp
expTH = [| 1 + 2 |]

-- >>>:t answer
-- answer :: Exp

-- >>>$(pure answer)
-- 32

-- >>>:t $(pure answer)
-- $(pure answer) :: Num p => p

answer :: Exp
answer = LitE (IntegerL 32)

-- >>>:t $hello
-- $hello :: IO ()

-- >>>runQ hello
-- AppE (VarE System.IO.putStrLn) (LitE (StringL "Hello Template Haskell"))

-- ->$hello
-- ->Hello Template Haskell
hello :: Q Exp
hello = [|putStrLn "Hello Template Haskell"|]

-- | typed Template Haskell
-- >>>$$(liftTyped (5::Int))
-- 5

-- >>>:t ( $$(liftTyped (5::Int)) )
-- ( $$(liftTyped (5::Int)) ) :: Int

-- >>>$$([|| 1 + 2 ||])
-- 3

-- >>>:t ([|| 1 + 2 ||])
-- ([|| 1 + 2 ||]) :: (Quote m, Num a) => Code m a
