{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module DerivingStrategyBasics where

import Data.Aeson ( ToJSON, encode ) 
import GHC.Generics 


newtype Age = Age {mage :: Int}
--  deriving (Show, Generic, Num, ToJSON) 
-- ^ this gives error since GHC choses @Num@ and @ToJson@ is derived with @AnyClass@ 
-- but Num should be derived with @newtype@
  deriving stock (Show, Generic)
  deriving newtype (Num)
  deriving anyclass (ToJSON)   
  
theAge :: Age
theAge = 33

-- | use case; 
-- >>> encodetoJSON
-- "{\"mage\":33}"
-- encodetoJSON :: bytestring-0.10.12.1:Data.ByteString.Lazy.Internal.ByteString
encodetoJSON = encode theAge

