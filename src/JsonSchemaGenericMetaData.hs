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
--{-# LANGUAGE PolyKinds #-}
--{-# LANGUAGE BangPatterns #-}

module JsonSchemaGenericMetaData where

import Control.Monad.Writer
import Data.Aeson (Value (..), (.=), object, Key)
import Data.Aeson.Key (fromText)
import qualified Data.Aeson.Encode.Pretty as AEP ( encodePretty )
import Data.Kind (Type, Constraint)
import Data.Text (Text, pack)
import Data.Typeable
import Data.Vector (fromList)
import GHC.Generics
import GHC.TypeLits
import qualified GHC.TypeLits as Err
import qualified Data.ByteString as BString
import qualified Data.ByteString.Lazy.Char8 as LC8 (putStrLn)

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

-- >>>testpp  
-- Object (fromList [("myproperty",Object (fromList [("type",String "boolean")]))])
testpp :: Value
testpp = makePropertObj @"myproperty" $ makeTypeObject @Bool

-- | pretty print for testing
prettyJSON :: Value -> IO ()
prettyJSON = LC8.putStrLn . AEP.encodePretty @Value 
          --(makePropertObj @"myproperty" $ makeTypeObject @Bool)

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

instance  (KnownSymbol nm, KnownSymbol (ToJSONType a) )
          => GSchema (M1 S ('MetaSel ('Just nm) _1 _2 _3)
                           (K1 _4 a)) where
  gschema = do
      emitRequired @nm
      pure . makePropertObj @nm $ makeTypeObject @a
  {-# INLINE gschema #-} 
  
  
-- | merging product of fields
instance (GSchema f , GSchema g) => GSchema (f :*: g) where
  gschema = mergeObjects <$> gschema @f <*> gschema @g
  {-# INLINE gschema #-}
  
-- | JSON Schema does give error message for sum types (not supported)
instance (TypeError ('Err.Text "JSON Schema does not support sum types")) 
          => GSchema (f :+: g) where
          gschema = error "JSON Schema does not support sum types"        
          {-# INLINE gschema #-}
-- | the value for data constructors will be reached 
-- thru M1 C - metadata for data constructors
instance GSchema a => GSchema (M1 C _1 a) where
    gschema = gschema @a
    {-# INLINE gschema #-}

-- | instance for M1 D type constructor is needed
-- to access the type's name and all of its properties
instance (GSchema a, KnownSymbol nm) 
      => GSchema (M1 D ('MetaData nm _1 _2 _3) a) where
    gschema = do
      sch <- gschema @a
      pure $ object 
        [ "title"      .= (String . pack . symbolVal $ Proxy @nm)
        , "type"       .= String "object"
        , "properties" .= sch
        ]
    {-# INLINE gschema #-}

schema :: forall a. (GSchema (Rep a), Generic a) => Value
schema = 
    let (v, reqs) = runWriter $ gschema @(Rep a)
    in mergeObjects v $ object
          [ "required" .= Array (fromList $ String <$> reqs) ]
{-# INLINE schema #-}

-- | instances for optional values, lists, strings
-- need to use overlapping instance for different base of M1...K1 to cover those

-- | first overlapping case; Maybe a
instance {-# OVERLAPPING #-} 
      (KnownSymbol nm, KnownSymbol (ToJSONType a))
          => GSchema (M1 S ('MetaSel ('Just nm) _1 _2 _3) (K1 _4 (Maybe a))) where
      gschema = pure . makePropertObj @nm $ makeTypeObject @a
      {-# INLINE gschema #-}

-- | Lists are serialized to Array in JSON Schema; 
-- their type is "array" and has extra property "items" and has a "type" property
-- { "type": "array", "items": { "type": "boolean" }}
-- need an Overlapping instance for K1 _ [a]
instance {-# OVERLAPPING #-} 
      (KnownSymbol nm, KnownSymbol (ToJSONType [a]), KnownSymbol (ToJSONType a) )
          => GSchema (M1 S ('MetaSel ('Just nm) _1 _2 _3) (K1 _4 [a])) where
      gschema = do
        emitRequired @nm
        let innerType = object 
                          [ "items" .= makeTypeObject @a
                          ]
        pure . makePropertObj @nm 
             . mergeObjects innerType 
             $ makeTypeObject @[a] 
      {-# INLINE gschema #-}

-- | special overlapping case needed for Strings since they are [Char]
-- it will be treated as an array unless there is a specialization
-- correct instance would be same as the default 'K1 _ a' case
instance {-# OVERLAPPING #-} KnownSymbol nm
    => GSchema (M1 S ('MetaSel ('Just nm) _1 _2 _3) (K1 _4 String)) where
  gschema = do
    emitRequired @nm
    pure . makePropertObj @nm $ makeTypeObject @String
  {-# INLINE gschema #-}


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
-- NOW WriterT (Identity ((),["required property"]))
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
type signature : (kv stands for type class KeyValue -> Pair = (Key, Value))
(.=) :: ToJSON v => Key -> v -> kv
-}
-- aeson package. also String is Text under the hood from the Value constructors
-- >>> makeTypeObject @Int
-- Object (fromList [("type",String "integer")])
makeTypeObject :: forall a . KnownSymbol (ToJSONType a) => Value
makeTypeObject = object ["type" .= String (pack . symbolVal $ Proxy @(ToJSONType a))]

-- | also need to define object with the name of a property
-- to be used the "properties"
-- NOTE: this does not work as described in the book. Author also left as undefined
-- added Key to the import list and used fromText 
-- >>>makePropertObj @"age" (makeTypeObject @String)
-- WAS WAS Object (fromList [("age",Object (fromList [("type",String "integer")]))])
-- WAS NOW Object (fromList [("age",Object (fromList [("type",String "string")]))])
-- NOW Object (fromList [("age",Object (fromList [("type",String "string")]))])

-- -> prettyJSON $ makePropertObj @"age" $ makeTypeObject @String
-- {
--     "age": {
--         "type": "string"
--     }
-- }
makePropertObj :: forall name . KnownSymbol name => Value -> Value
makePropertObj v = object [fromText (pack (symbolVal $ Proxy @name)) .= v ]

-- >>>:kind! Rep PersonN
-- Rep PersonN :: * -> *
-- = D1
--     ('MetaData "PersonN" "JsonSchemaGenericMetaData" "main" 'False)
--     (C1
--        ('MetaCons "PersonN" 'PrefixI 'True)
--        ((S1
--            ('MetaSel
--               ('Just "name")
--               'NoSourceUnpackedness
--               'NoSourceStrictness
--               'DecidedLazy)
--            (Rec0 String)
--          :*: S1
--                ('MetaSel
--                   ('Just "age")
--                   'NoSourceUnpackedness
--                   'NoSourceStrictness
--                   'DecidedLazy)
--                (Rec0 Int))
--         :*: (S1
--                ('MetaSel
--                   ('Just "phone")
--                   'NoSourceUnpackedness
--                   'NoSourceStrictness
--                   'DecidedLazy)
--                (Rec0 (Maybe String))
--              :*: S1
--                    ('MetaSel
--                       ('Just "permissions")
--                       'NoSourceUnpackedness
--                       'NoSourceStrictness
--                       'DecidedLazy)
--                    (Rec0 [Bool]))))

-- | Finally completed; (pasted from ghci)
{- 
  λ >> prettyJSON  (schema @PersonN)
{
    "properties": {
        "age": {
            "type": "integer"
        },
        "name": {
            "type": "string"
        },
        "permissions": {
            "items": {
                "type": "boolean"
            },
            "type": "array"
        },
        "phone": {
            "type": "string"
        }
    },
    "required": [
        "name",
        "age",
        "permissions"
    ],
    "title": "PersonN",
    "type": "object"
}








-}