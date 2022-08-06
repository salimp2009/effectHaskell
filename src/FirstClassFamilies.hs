{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
module FirstClassFamilies where
import TypeLevelDefunctionalization (Exp, Evaltf, Snd, FromMaybe)

-- | First Class families form a monad a t type level

-- >>>:kind! Evaltf (Pure 5)
-- Evaltf (Pure 5) :: Nat
-- = 5
data Pure :: a -> Exp a 
type instance Evaltf (Pure x) = x


data (=<<) :: (a -> Exp b) -> Exp a -> Exp b
infixr 0 =<<
type instance Evaltf (f =<< g) = Evaltf (f (Evaltf g ))

-- | to compose; the traditional Fish operator (<=<) ; Kleisli composition
-- is used to represent this combinator.
-- but at type level we can not use the the dot operator 
-- since it conflicts with forall quantifier
type Snd2 = Snd <=< Snd

-- >>>:kind! Evaltf (Snd2 '(1, '(2, 3)))
-- Evaltf (Snd2 '(1, '(2, 3))) :: Nat
-- = 3

-- >>>:kind! Evaltf (Snd2 '(1, '(2, "salitos")))
-- Evaltf (Snd2 '(1, '(2, "salitos"))) :: Symbol
-- = "salitos"

-- >>>:kind! Evaltf (Snd <=< FromMaybe '(0, 0) =<< Pure (Just '(1, 2)))  
-- Evaltf (Snd <=< FromMaybe '(0, 0) =<< Pure (Just '(1, 2))) :: Nat
-- = 2

--- >>>:kind! Evaltf (Snd <=< FromMaybe '(0, "salikos") =<< Pure Nothing) 
-- Evaltf (Snd <=< FromMaybe '(0, "salikos") =<< Pure Nothing) :: Symbol
-- = "salikos"
data (<=<) :: (b -> Exp c) -> (a -> Exp b) -> a -> Exp c
infixr 1 <=<
type instance Evaltf ((f <=< g) x) = Evaltf (f (Evaltf (g x)))
