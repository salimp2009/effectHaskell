{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
module WebAPI3 where

import Control.Applicative ((<|>))
import WebAPI2
import WebAPI1
import Data.Proxy (Proxy)

{- 
  "The FlexibleInstances GHC extension is almost always needed 
  when we define instances. Standard requirements for instances are very tight. 
  eg; they allow only instances with the C (T a1 … an) head, where C is the class, 
  T is a data type constructor, and the a1 … an are distinct type variables. 
  FlexibleInstances allows arbitrary nested types instead"
-}

-- | the goal is implement route function based on the types of parameters
class HasServer layout where
  routeS :: Proxy layout          -- << gives the type of API
         -> Server layout         -- << API implementation
         -> Request               -- << list of requests
         -> Maybe (IO String)     -- << resulting action

instance Show a => HasServer (Get a) where
  routeS :: Proxy (Get a) -> HandlerAction a -> Request -> Maybe (IO String) 
  routeS _ handler [] = Just (encode $ handler )
  routeS _ _       _  = Nothing



