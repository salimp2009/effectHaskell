{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DerivingStrategies #-}
module GenericDataTypeBasics where

import GHC.Generics 
import Data.Kind (Type)
   
-- | fact about the type
-- defined in module GenericDataTypeBasics; used by GHC
-- it is data type ; not a new type
-- has 2 constructors; 
-- Left (first) constructor called "OK"; it has no arguments; not a record selector, in Prefix form
-- Right (second) const called "Err"; it has no arguments; not a record selector, in Prefix form
data Status = OK | Err
    deriving (Show, Generic)

-- | Definition of Generic Class    
-- class Generic a where
--     type Rep a :: Type -> Type
--     from :: a -> Rep a x
--     to   :: Rep a x -> a 

data Status2 s = OKK | Er s
    deriving (Show, Generic)

-- >>>from OKK
-- M1 {unM1 = L1 (M1 {unM1 = U1})}

-- >>>from (Er (Just 5))
-- M1 {unM1 = R1 (M1 {unM1 = M1 {unM1 = K1 {unK1 = Just 5}}})}

-- >>> to errVal::Status
-- Err
errVal :: M1 i1 c1 (f :+: M1 i2 c2 U1) p
errVal = M1 {unM1 = R1 (M1 {unM1 = U1})}

-- >>> from errVal2
-- M1 {unM1 = R1 (M1 {unM1 = U1})}
errVal2 :: Status
errVal2 = Err

-- >>>:t from OK
-- from OK
--   :: D1
--        ('MetaData "Status" "GenericDataTypeBasics" "main" 'False)
--        (C1 ('MetaCons "OK" 'PrefixI 'False) U1
--         :+: C1 ('MetaCons "Err" 'PrefixI 'False) U1)
--        x

-- >>> from OK
-- M1 {unM1 = L1 (M1 {unM1 = U1})}

-- >>> from Err
-- M1 {unM1 = R1 (M1 {unM1 = U1})}

-- | Generic Representation of a product type
-- runtime values are represented with K1 data constructor
-- there is also Generic1 type class with DeriveGeneric
-- which represents type constructors of k -> Type kinds
-- >>> from (Request "sal" 1)
-- M1 {unM1 = M1 {unM1 = M1 {unM1 = K1 {unK1 = "sal"}} :*: M1 {unM1 = K1 {unK1 = 1}}}}

data Request = Request String Int
    deriving Generic

-- >>>:kind! Rep Request
-- Rep Request :: * -> *
-- = D1
--     ('MetaData "Request" "GenericDataTypeBasics" "main" 'False)
--     (C1
--        ('MetaCons "Request" 'PrefixI 'False)
--        (S1
--           ('MetaSel
--              'Nothing 'NoSourceUnpackedness 'NoSourceStrictness 'DecidedLazy)
--           (Rec0 String)
--         :*: S1
--               ('MetaSel
--                  'Nothing 'NoSourceUnpackedness 'NoSourceStrictness 'DecidedLazy)
--               (Rec0 Int)))

-- >>>:t M1 
-- M1 :: f p -> M1 i c f p

-- >>>:k S1
-- S1 :: Meta -> (k -> *) -> k -> *

