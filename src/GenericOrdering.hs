-- {-# LANGUAGE DefaultSignatures #-}
-- {-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
-- {-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}

module GenericOrdering where
import GHC.Generics    

class GOrd a where
    gord :: a x -> a x -> Ordering  
    
instance GOrd U1 where
    gord U1 U1 = EQ
    
instance GOrd V1 where
    gord _ _ = EQ  

instance (Ord a) => GOrd (K1 _1 a ) where
    gord (K1 a) (K1 b) = compare a b

instance (GOrd a, GOrd b) => GOrd (a :+: b) where 
    gord (L1 a1) (L1 a2)  = gord a1 a2 
    gord (R1 b1) (R1 b2)  = gord b1 b2
    gord (L1 _) (R1 _)    = LT
    gord (R1 _) (L1 _)    = GT  

instance (GOrd a, GOrd b) => GOrd (a :*: b) where
    gord (a1 :*: b1) (a2 :*: b2) = gord a1 a2 <> gord b1 b2

instance GOrd a => GOrd (M1 _1 _2 a) where
    gord (M1 a1) (M1 a2) = gord a1 a2 

-- | use cases;
-- >>> genericOrd (Left 5) (Left 1)       
-- GT
-- >>> genericOrd (Nothing) (Just 500)
-- LT
-- >>> genericOrd (Nothing) (Nothing) 
-- EQ
genericOrd :: (Generic a, GOrd(Rep a)) => a -> a -> Ordering
genericOrd a b = gord (from a) (from b)

data Foo1 a b c = FF0
                | FF1 a
                | FF2 b c
                deriving (Eq, Generic)
-- | use cases;
-- >>> compare (FF1 5) (FF1 3)
-- GT
-- >>> compare (FF1 2) (FF2 3 5)
-- LT
-- >>> compare (FF2 1 6) (FF2 3 5)
-- LT
instance (Ord a, Ord b, Ord c) => Ord (Foo1 a b c ) where
    compare = genericOrd
    
