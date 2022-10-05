module WebAPI1 where
import Data.Kind (Type)

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