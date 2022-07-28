{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ExistentialTypes where

import Type.Reflection (Typeable(..))
import Data.Typeable(cast)
import Data.Maybe(fromMaybe)
import Data.Foldable (asum)

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

-- | similar to regular Show instance but using the eliminator as an exercise
-- instance Show HasShow where
--     show x  = "HasShow " <> elimHasShow show x

-- | attemp to create Python 
data Dynamic where
  Dynamic :: Typeable t => t -> Dynamic
  
elimDynamic :: (forall a. Typeable a => a -> r) -> Dynamic -> r
elimDynamic f (Dynamic a )  = f a

-- | getting existential value out of Dynamic
-- cast is from Data.Typeable and signature
-- cast :: forall a b. (Typeable a, Typeable b) => a -> Maybe b
-- we pass cast into elimDynamic so it passes the value inside Dynamic type constructor
fromDynamic2 :: Typeable a => Dynamic -> Maybe a
fromDynamic2  = elimDynamic cast

-- | lift is used to pass a binary operation on runtime values like Python style
liftDyn2 :: forall a b r.(Typeable a, Typeable b, Typeable r) 
            => Dynamic -> Dynamic -> ( a -> b -> r) -> Maybe Dynamic
liftDyn2 d1 d2 f= fmap Dynamic . f <$> fromDynamic2 @a d1 <*> fromDynamic2 @b d2

-- | use case;
-- >>>default (Int)
-- >>>fromDynamic2 @Int (pyPlus (Dynamic 1) (Dynamic 2))
-- Just 3

-- >>> fromDynamic2 @String (pyPlus (Dynamic (1::Int)) (Dynamic "sal"))
-- Just "1 sal"

-- >>> fromDynamic2 @String (pyPlus (Dynamic (1::Int)) (Dynamic "sal")) 
-- Just "1 sal"
pyPlus :: Dynamic -> Dynamic -> Dynamic
pyPlus a b = fromMaybe (error "bad types forPyPlus") $ asum
  [ liftDyn2 @String @String a b (++)
  , liftDyn2 @Int @Int a b (+)  
  , liftDyn2 @String @Int a b $ \strA intB -> strA <>" " <> show intB
  , liftDyn2 @Int @String a b $ \intA strB -> show intA <> " " <> strB
  ] 
