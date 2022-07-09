{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}

module GenericOrdering where
import GHC.Generics    

class GOrd a where
    gord :: a x -> a x -> Ordering  
    
instance GOrd U1 where
    gord U1 U1 = EQ
    
instance GOrd V1 where
    gord _ _ = EQ  

instance (Eq a, Ord a) => GOrd (K1 _1 a ) where
    gord (K1 a) (K1 b) = compare a b