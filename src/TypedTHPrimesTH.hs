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
module TypedTHPrimesTH where


import Language.Haskell.TH
import Language.Haskell.TH.Syntax

import TypedTHPrimes (isPrime)

{- 
  type role TExp nominal
  newtype TExp (a :: TYPE (r :: RuntimeRep)) = TExp
    { unType :: Exp }
-}

-- >>>:i Code
-- type role Code representational nominal
-- type Code :: (* -> *) -> * -> *
-- newtype Code m a = Code {examineCode :: m (TExp a)}
--   	-- Defined in ‘Language.Haskell.TH.Syntax’

-- Before GHC 9 instead of Code
-- primesUpTo' :: Integer -> Q (TExp [Integer])
primesUpTo' :: Integer -> Code Q [Integer]
primesUpTo' n = go 2
  where
    go i
      | i > n     = [|| [] ||]
      | isPrime i = [|| i : $$(go (i+1)) ||]
      | otherwise = [|| $$(go (i+1)) ||]

-- >>>$$(primesUpTo' 100)    
-- [2,3,5,7,11,13,17,19,23,25,29,31,35,37,41,43,47,49,53,55,59,61,65,67,71,73,77,79,83,85,89,91,95,97]

-- >>>$$(primesUpTo' 10)
-- [2,3,5,7]

-- >>>runQ (unType <$> examineCode (primesUpTo' 10))
-- InfixE (Just (LitE (IntegerL 2))) (ConE GHC.Types.:) 
--        (Just (InfixE (Just (LitE (IntegerL 3))) (ConE GHC.Types.:) 
--        (Just (InfixE (Just (LitE (IntegerL 5))) (ConE GHC.Types.:) 
--        (Just (InfixE (Just (LitE (IntegerL 7))) (ConE GHC.Types.:) 
--        (Just (ConE GHC.Types.[]))))))))




