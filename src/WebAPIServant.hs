{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}

module WebAPIServant (app) where

import GHC.Generics
import Data.Aeson
import Servant
import Servant.HTML.Blaze
import qualified Text.Blaze.Html5 as H
import Network.Wai.Handler.Warp

-- | Using Servant to implement web services
-- warp as an HTTP Server
-- "blaze-html and servant-blaze to generate and return HTML documents"
-- aeson for JSON related 

data RatingS = Bad | Good | Great 
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

type BookIDS = Int

data ServiceStatusS = Ok | Down
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

type BookInfoAPIS = Get '[JSON] ServiceStatusS
                      :<|> "title"   :> Capture "id" BookIDS :> Get '[HTML] H.Html
                      :<|> "year"    :> Capture "id" BookIDS :> Get '[JSON] Int
                      :<|> "rating"  :> Capture "id" BookIDS :> Get '[JSON] RatingS

implS :: Server BookInfoAPIS
implS = pure Ok
      :<|> titleS
      :<|> yearS
      :<|> ratingS
  where
    titleS  _ = pure $  H.toHtml $ H.b "Haskell in Depth"
    yearS   _ = pure 2021 
    ratingS _ = pure Great

app :: Application
app = serve (Proxy :: Proxy BookInfoAPIS) implS

-- >>>:i run    
-- run :: Port -> Application -> IO ()
--   	-- Defined in ‘Network.Wai.Handler.Warp.Run’

-- >>>:i Handler    
-- type role Handler nominal
-- type Handler :: * -> *
-- newtype Handler a
--   = Handler {runHandler' :: ExceptT ServerError IO a}
--   	-- Defined in ‘Servant.Server.Internal.Handler’

-- >>>:t Handler
-- Handler :: ExceptT ServerError IO a -> Handler a

-- >>>:t implS
-- implS
--   :: Handler ServiceStatusS
--      :<|> ((Int -> Handler (MarkupM ()))
--            :<|> ((Int -> Handler Int) :<|> (Int -> Handler RatingS)))

-- >>>:i Capture
-- type Capture :: Symbol -> * -> *
-- type Capture = Capture' '[] :: Symbol -> * -> *
--   	-- Defined in ‘Servant.API.Capture’

-- >>>:i Capture'
-- type role Capture' phantom phantom phantom
-- type Capture' :: [*] -> Symbol -> * -> *
-- data Capture' mods sym a
--   	-- Defined in ‘Servant.API.Capture’

