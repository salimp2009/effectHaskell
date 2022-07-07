{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
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
--     type Rep a = Type -> Type
--     from :: a -> Rep a x
--     to   :: Rep a x -> a 


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


