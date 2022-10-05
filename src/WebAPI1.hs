{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
module WebAPI1 where

import Text.Read (readMaybe)

-- | Book rating and services can be represented enumarating the 
-- potential values
data Rating = Bad | Good | Great
  deriving Show

data ServiceStatus = Ok | Down
  deriving Show

-- | Request handlers will be represented will specific
-- type synonym rather than plain String
type BookID          = Int
type HandlerAction a = IO a
type ReqHandler a    = BookID -> HandlerAction a

data BookInfoAPIImpl = BookInfoAPIImpl 
                        { root   :: HandlerAction ServiceStatus
                        , title  :: ReqHandler String
                        , year   :: ReqHandler Int
                        , rating :: ReqHandler Rating
                        }

impl1 :: BookInfoAPIImpl
impl1 = BookInfoAPIImpl
         { root   = pure Ok
         , title  = \_ -> pure "Haskell In Depth" 
         , year   = \_ -> pure 2021
         , rating = \_ -> pure Great
         }       
         
impl2 :: BookInfoAPIImpl
impl2 = BookInfoAPIImpl
         { root   = pure Down
         , title  = const notImplemented 
         , year   = const notImplemented
         , rating = const notImplemented
         }            
    where 
      notImplemented = ioError (userError "not implemented")

type Request = [String]

encode :: Show a => HandlerAction a -> IO String
encode m = show <$> m 

route :: BookInfoAPIImpl -> Request -> Maybe (IO String)
route impl [] = pure $ encode $ root impl
route impl [op, bid'] = do
      bid <- readMaybe bid'
      case op of
        "title"  -> pure $ encode $ title impl bid
        "year"   -> pure $encode $ year impl bid
        "rating" -> pure $encode $ rating impl bid
        _        -> Nothing
route _ _ = Nothing

-- >>>getReq2 impl1 ["year", "7548"]
-- "2021"

-- >>>getReq2 impl2 ["year", "7548"]
-- user error (not implemented)

-- >>>getReq2 impl1 []
-- "Ok"

-- >>>getReq2 impl2 []
-- "Down"
getReq2 :: BookInfoAPIImpl -> Request -> IO String
getReq2 impl reqs = 
    case route impl reqs of
      Just m  -> m
      Nothing -> pure "Malformed request"

checkReq2 ::  BookInfoAPIImpl  -> IO ()     
checkReq2 impl = do
   rootOK <- getReq2 impl []
   answer <- getReq2 impl ["year", "7548"]
   putStrLn ( if rootOK == "Ok" && answer == "2021"
              then "OK"
              else "Wrong anser"
            )
