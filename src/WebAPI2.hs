{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
module WebAPI2 where

import Data.Kind
import GHC.TypeLits
import Text.Read (readMaybe)

import WebAPI1
    ( encode,
      BookID,
      HandlerAction,
      Rating(Great),
      Request,
      ServiceStatus(Ok) )

-- | describes results ; what we are getting 
-- as a response
data Get (a::Type)

-- |captures a request parameter
data Capture (a::Type)

-- | lists the alternative request operations
data a :<|> b = a :<|> b
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

-- | layout stands for our BookInfoAPI 
-- Server layout -> implementation 
type family Server layout :: Type  
type instance Server (Get a)        = HandlerAction a
type instance Server (a :<|> b)     = Server a :<|> Server b
type instance Server ((s::Symbol) :> r) = Server r
type instance Server (Capture a :> r)   = a -> Server r

implServer :: Server BookInfoAPI
implServer = pure Ok
            :<|> title
            :<|> year
            :<|> rating
      where
       title _  = pure "Haskell in Depth"
       year _   = pure 2021
       rating _ = pure Great
       
-- | same as above since this directly applies implementation
-- the above version uses type family to directly work on types
-- and creates the same implementation 
implServer2 :: BookInfoAPIImpl2
implServer2 = pure Ok
            :<|> (\_ -> pure "Haskell in Depth" )
            :<|> (\_ -> pure 2021 )
            :<|> (\_ -> pure Great )

route2 :: Server BookInfoAPI -> Request -> Maybe (IO String)            
route2 (root :<|> _) [] = pure $ encode $ root
route2 (_ :<|> title :<|> year  :<|> rating) [op, bid'] = do
      bid   <- readMaybe bid'
      case op of 
            "title"  -> pure $ encode $ title bid
            "year"   -> pure $ encode $ year bid
            "rating" -> pure $ encode $ rating bid
            _        -> Nothing
route2 _ _ = Nothing    

getReq3 :: Server BookInfoAPI -> Request -> IO String
getReq3 impl reqs =
      case route2 impl reqs of
            Just m  -> m
            Nothing -> pure "Malformed request"  