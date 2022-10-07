{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
module WebAPI3 where

import Control.Applicative ((<|>))
import WebAPI2
import WebAPI1
import Data.Proxy (Proxy(..), Proxy)
import GHC.TypeLits

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

instance {-# OVERLAPS #-} HasServer (Get String) where  
  routeS :: Proxy (Get String) 
         -> IO String
         -> Request
         -> Maybe (IO String)
  routeS _ handler [] = Just handler 
  routeS _ _       _  = Nothing        


instance (HasServer a, HasServer b) => HasServer (a :<|> b) where 
  routeS :: Proxy (a :<|> b) 
         -> (Server a :<|> Server b) 
         -> Request
         -> Maybe (IO String)
  routeS _ (handlera :<|> handlerb) xs = 
         routeS (Proxy :: Proxy a) handlera xs
     <|> routeS (Proxy :: Proxy b) handlerb xs

instance  (KnownSymbol s, HasServer r) => HasServer ((s::Symbol) :> r)   where
  routeS :: Proxy (s :> r) -> Server r -> Request -> Maybe (IO String)
  routeS _ handler (x : xs)
      | symbolVal (Proxy :: Proxy s) == x = routeS (Proxy :: Proxy r) handler xs
  routeS _ _      _ =  Nothing  


