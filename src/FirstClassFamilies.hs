{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
module FirstClassFamilies where
import TypeLevelDefunctionalization (Exp, Evaltf, Snd)

-- | First Class families form a monad a t type level

-- >>>:kind! Evaltf (Pure 5)
-- Evaltf (Pure 5) :: Nat
-- = 5
data Pure :: a -> Exp a 
type instance Evaltf (Pure x) = x

-- | the traditional Fish operator (<=<) ; Kleisli composition
-- is used to represent this combinator but at type level 
-- it conflicts with forall quantifier
data (=<<) :: (a -> Exp b) -> Exp a -> Exp c
infixr 0 =<<
type instance Evaltf (f =<< g) = Evaltf (f (Evaltf g ))


