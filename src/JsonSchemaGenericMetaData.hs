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

-- | target is to create a JSON Schema like;
{-
      { "title": "Person"
    , "type": "object"
    , "properties":
    { "name": { "type": "string" }
    , "age": { "type": "integer" }
    , "phone": { "type": "string" }
    , "permissions":
    { "type": "array", "items": { "type": "boolean" }}
    }
    , "required": ["name" , "age", "permissions"]
    }
-}
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

-- | Value is a JSON value as a Haskell value from aeson package
{-
  data Value = Object !Object
           | Array !Array
           | String !Text
           | Number !Scientific
           | Bool !Bool
           | Null
             deriving (Eq, Read, Typeable, Data, Generic)
-}  
mergeObjects :: Value -> Value -> Value
mergeObjects (Object a) (Object b) =   Object $ a <> b
mergeObjects _ _ = error "unsafe use of mergeObjects"

-- | use case;
-- >>>emitRequired @"required property"
-- WriterT (Identity ((),["required property"]))
emitRequired :: forall nm. KnownSymbol nm => Writer [Text] ()
emitRequired = tell . pure . pack . symbolVal $ Proxy @nm
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

-- >>>:kind! ToJSONType (Maybe Int)
-- ToJSONType (Maybe Int) :: Symbol
-- = "Maybe"

-- >>>:t from (Just 5)
-- from (Just 5)
--   :: Num a =>
--      D1
--        ('MetaData "Maybe" "GHC.Maybe" "base" 'False)
--        (C1 ('MetaCons "Nothing" 'PrefixI 'False) U1
--         :+: C1
--               ('MetaCons "Just" 'PrefixI 'False)
--               (S1
--                  ('MetaSel
--                     'Nothing 'NoSourceUnpackedness 'NoSourceStrictness 'DecidedLazy)
--                  (Rec0 a)))
--        x
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

-- | to generate objects like {"type": "foo"}
-- we need another function using aeson paackage's object function
-- operator (.=) is defined as;
{- " Retrieve the value associated with the given key of an 'Object'.
-- The result is 'empty' if the key is not present or the value cannot
-- be converted to the desired type."
type signature : 
(.:) :: (FromJSON a) => Object -> Key -> Parser a
-} 
-- aeson package. also String is Text under the hood from the Value constructors
-- >>> makeTypeObject @Int
-- Object (fromList [("type",String "integer")])
makeTypeObject :: forall a . KnownSymbol (ToJSONType a) => Value
makeTypeObject = object ["type" .= String (pack . symbolVal $ Proxy @(ToJSONType a))]

-- | also need to define object with the name of a property
-- to be used the "properties"
-- NOTE: this does not work as described in the book. Author also left as undefined
makePropertObj :: forall name . KnownSymbol (ToJSONType name) => Value -> Value
makePropertObj v = undefined
  -- object [pack (symbolVal $ Proxy @name) .= v ]



