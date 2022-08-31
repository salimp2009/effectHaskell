{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -O -fplugin Test.Inspection.Plugin #-}
module InpsectionTesting where

import Data.Aeson ( Value )
import JsonSchemaGenericMetaData ( schema, PersonN )
import Test.Inspection ( hasNoGenerics, inspect )

-- | testing if Generics causes extra overhead
-- testSchema01 is just any function that used our generics implementation
-- then pass it to   
-- inspect $ hasNoGenerics 'testSchema01
-- if it compiles OK then overhead at an unacceptable level
-- if there is any runtime overhead GHC will refuse to continue
-- NOTE that this is not a proof of Generics implementation is
-- as good as handwritten or GHC derived version 
-- another way to optimize to use Template Haskell
-- to generate code for Generic implementation
testSchema01 :: Value
testSchema01 = schema @PersonN

inspect $ hasNoGenerics 'testSchema01

{-
  to prove two implementations 
  e.g.; generically and one written by hand) are equal,
  spection-testing’s (===) combinator can be used. 
  ===) causes a compile-time error if the actual generate Core
  sn’t identical. 
  This is often impractical to do for
  omplicate usages of GHC.Generics, but it’s comforting to
  now that it’s possible in principle."
-}