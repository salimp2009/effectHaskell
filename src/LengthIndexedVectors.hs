--{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
module LengthIndexedVectors where
import Data.Kind (Type)
-- import Prelude hiding (and)

-- | this is not the Nat we use in TypeLits
-- used only for the example and not a very efficient one
data Nat = Zero | Succ Nat
  deriving (Show)

type Vec n a = VectorID n a

-- >>>:t (3:> 4:> 5:> VNil)
-- (3:> 4:> 5:> VNil) :: Num a => VectorID ('Succ ('Succ ('Succ 'Zero))) a
type VectorID :: Nat -> Type -> Type  
data VectorID n a where
  VNil  :: VectorID 'Zero a
  -- VCons :: a -> VectorID n a -> VectorID ('Succ n) a   -- Original implementation from "Production Haskell"
  (:>) ::  a -> VectorID n a -> VectorID ('Succ n) a      -- << added later as a fancy constructor :)
infixr 5 :>

deriving stock instance Show a => Show (VectorID n a)

-- | Original Show instance; above is a fancier way
-- instance Show a => Show (VectorID n a) where
--   show VNil = "VNil"
--   show (VCons a as) = "VCons " <> show a <> " (" <> show as <> ")"

-- >>>append  (1 :> 3 :> VNil) (2 :> VNil)  
-- 1 :> (3 :> (2 :> VNil))
append :: Vec n a -> Vec m a -> Vec (Add n m) a  
append VNil v2 = v2
-- append (VCons a rest) v2 = VCons a (append rest v2)  -- << Original implementation
append (a :> rest ) v2 = a :> append rest v2

-- >>>addNat (Succ(Succ Zero)) (Succ Zero)
-- Succ (Succ (Succ Zero))
addNat :: Nat -> Nat -> Nat
addNat Zero m = m
addNat (Succ nat) m = addNat nat (Succ m)

-- >>>:kind! Add ('Succ('Succ 'Zero)) ('Succ 'Zero)
-- Add ('Succ('Succ 'Zero)) ('Succ 'Zero) :: Nat
-- = 'Succ ('Succ ('Succ 'Zero))
type family Add (x::Nat) (y::Nat) where
  Add 'Zero n = n
  Add ('Succ n) m = 'Succ (Add n m)
-- >>> andVec (True :> False :> VNil)  
-- False

-- >>> andVec (True :> True :> VNil) 
-- True
andVec :: Vec n Bool -> Bool
andVec VNil      = True
andVec (b :> bs) = b && andVec bs

-- >>>headVec $ append (1 :> 2 :> VNil) (3 :> 4 :> 5 :> VNil) 
-- 1
headVec :: Vec ('Succ n) a -> a
headVec (x :> _) = x

-- | similar init in Data.List drops the last elem in the list
-- only accepts non-empty list as impplied by type annotation
initVec :: Vec (Succ n) a -> Vec n a 
initVec (_ :> VNil)        = VNil
initVec (x :> xs@(_ :> _)) = x :> initVec xs

mapVec :: (a -> b) -> Vec n a -> Vec n b
mapVec _ VNil      = VNil
mapVec f (x :> xs) = f x :> mapVec f xs

