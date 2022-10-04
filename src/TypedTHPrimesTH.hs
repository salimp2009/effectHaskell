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
{-# LANGUAGE RankNTypes #-}
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
      | isPrime i = [|| i : $$(go (i + 1)) ||]
      | otherwise = [|| $$(go (i + 1)) ||]

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


-- >>>:t runQ 
-- runQ :: Quasi m => Q a -> m a

primesUpTo2 :: Integer -> [Integer]
primesUpTo2 n = filter isPrime [2..n]

primesUpTo2' :: Integer -> Code Q [Integer]
primesUpTo2' n = [|| primesUpTo2 n ||]

mempty' :: forall a. Monoid a => Code Q a 
mempty' = [|| mempty||]

-- >>>:t mempty
-- mempty :: Monoid a => a


-- >>> myXempty :: [Int] ; myXempty = id $$(mempty' @([Int]))
-- >>> myXempty
-- []

-- >>> myXempty :: String ; myXempty = id $$(mempty' :: Code Q String)
-- >>> myXempty
-- ""

-- | if such an error exists then do type application or explicit type declaration
-- as show above examples of myXempty
-- >>> myXempty :: String ; myXempty = id $$mempty'
-- >>> myXempty
-- Ambiguous type variable ‘a0’ arising from a use of ‘mempty'’
-- prevents the constraint ‘(Monoid a0)’ from being solved.
-- Probable fix: use a type annotation to specify what ‘a0’ should be.
-- These potential instances exist:
--   instance Monoid Series -- Defined in ‘Data.Aeson.Encoding.Internal’
--   instance Monoid Key -- Defined in ‘Data.Aeson.Key’
--   instance Monoid (KeyMap v) -- Defined in ‘Data.Aeson.KeyMap’
--   ...plus 81 others
--   (use -fprint-potential-instances to see them all)

-- | it is also possible to convert from old style to new style 
-- from Q (TExp a) -> Code Q a
-- >>>:t liftCode 
-- liftCode :: m (TExp a) -> Code m a

-- >>>:t Code
-- Code :: m (TExp a) -> Code m a

-- >>>:t Q
-- Q :: (forall (m :: * -> *). Quasi m => m a) -> Q a

-- >>>:i Quote
-- type Quote :: (* -> *) -> Constraint
-- class Monad m => Quote m where
--   newName :: String -> m Name
--   {-# MINIMAL newName #-}
--   	-- Defined in ‘Language.Haskell.TH.Syntax’
-- instance Quote Q -- Defined in ‘Language.Haskell.TH.Syntax’
-- instance Quote IO -- Defined in ‘Language.Haskell.TH.Syntax’















