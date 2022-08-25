{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module JsonSchemaGenericMetaData where

import Control.Monad.Writer
import Data.Aeson (Value (..), (.=), object)
import Data.Kind (Type, Constraint)
import Data.Text (Text, pack)
import Data.Typeable
import Data.Vector (fromList)
import GHC.Generics
import GHC.TypeLits
import qualified GHC.TypeLits as Err
import Data.Vector.Primitive (Vector(Vector))

data PersonN = PersonN
  { name  :: String
  , age   :: Int
  , phone :: Maybe String
  , permissions :: [Bool]    
  }
  deriving (Generic)

type GSchema :: (Type -> Type ) -> Constraint 
class GSchema a where
  gschema :: Writer [Text] Value

mergeObjects :: Value -> Value -> Value
mergeObjects (Object a) (Object b) =   Object $ a <> b
mergeObjects _ _ = error "unsafe use of mergeObjects"

-- | use case;
-- >>>emitRequired @"required property"
-- WriterT (Identity ((),["required property"]))
emitRequired :: forall nm. KnownSymbol nm => Writer [Text] ()
emitRequired = tell . pure . pack. symbolVal $ Proxy @nm
-- emitRequired = do
--           let s = pack . symbolVal $ Proxy @nm
--           tell (pure s)

-- | use case;
-- >>>:kind! ToJSONType Int
-- ToJSONType Int :: Symbol
-- = "integer"

-- >>>:kind! ToJSONType String
-- ToJSONType String :: Symbol
-- = "string"

-- >>>:kind! ToJSONType [Int]
-- ToJSONType [Int] :: Symbol
-- = "array"

-- >>>:kind! ToJSONType PersonN
-- ToJSONType PersonN :: Symbol
-- = "PersonN"
type family ToJSONType (a::Type)::Symbol where
  ToJSONType Int      = "integer"
  ToJSONType Integer  = "integer"
  ToJSONType Float    = "number"  
  ToJSONType Double   = "number"  
  ToJSONType String   = "string"  
  ToJSONType Bool     = "boolean"  
  ToJSONType [a]      = "array"  
  ToJSONType a        = TypeName a 
  
type family RepName (x :: Type -> Type) :: Symbol where
  RepName (D1 ('MetaData nm _ _ _) _) = nm

type family TypeName (x :: Type) :: Symbol where  
  TypeName t = RepName (Rep t)

      


