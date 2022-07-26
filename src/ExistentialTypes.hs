{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
--{-# LANGUAGE TypeApplications #-}
module ExistentialTypes where

import Type.Reflection (Typeable)

data Any where
  Any :: Show a => a -> Any

-- | use case;
-- >>>Any 5  
-- Any 5
instance Show Any where
  show (Any x) = "Any " <> show x

-- | use case
-- >>>elimAny show (Any 5)
-- "5"
elimAny :: forall r . (forall a. Show a => a -> r)  -> Any -> r
elimAny f (Any x) =  f x 

data HasShow where
  HasShow :: Show t => t -> HasShow

-- | use case;
-- >>>HasShow 5  
-- HasShow 5
instance Show HasShow where
    show (HasShow x)  = "HasShow " <> show x

-- | use case;
-- >>>elimHasShow show (HasShow "salitos")    
-- "\"salitos\""
elimHasShow :: (forall a. Show a => a -> r) -> HasShow -> r
elimHasShow f (HasShow x) = f x

-- instance Show HasShow where
--     show x  = "HasShow " <> elimHasShow show x


