{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
module WebAPI2 where

import Data.Kind
import GHC.TypeLits

import WebAPI1

-- | describes results ; what we are getting 
-- as a response
data Get (a::Type)

-- |captures a request parameter
data Capture (a::Type)

-- | lists the alternative request operations
data a :<|> b = a:<|> b
infixr 8 :<|>

-- |
data (a :: k) :> (b :: Type)
infixr 9 :> 

type BookInfoAPI =
      Get ServiceStatus
    :<|> "title" :> Capture BookID :> Get String
    :<|> "year"  :> Capture BookID :> Get Int
    :<|> "rating"  :> Capture BookID :> Get Rating

type BookInfoAPIImpl2 =
        HandlerAction ServiceStatus
  :<|> (BookID -> HandlerAction String)     
  :<|> (BookID -> HandlerAction Int)     
  :<|> (BookID -> HandlerAction Rating)     