{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
module FirstClassFamilyAdHOCPolymorphism where

import TypeLevelDefunctionalization (Exp, Evaltf, Snd)
import Data.Kind (Type, Constraint)
import GHC.TypeLits (type (+), Nat)


-- | use case for Map at type level
-- >>>:kind! Evaltf (Mapt Snd ('Just '(1, 2)))
-- Evaltf (Mapt Snd ('Just '(1, 2))) :: Maybe Nat
-- = 'Just 2

-- >>>:kind! Evaltf (Mapt (Mapt Snd) '[ ('Just '(1, 2)), ('Just '(3, 4))] )
-- Evaltf (Mapt (Mapt Snd) '[ ('Just '(1, 2)), ('Just '(3, 4))] ) :: [Maybe
--                                                                      Nat]
-- = '[ 'Just 2, 'Just 4]

-- >>>:kind! Evaltf (Mapt Snd ('Left '(1, 2)))
-- Evaltf (Mapt Snd ('Left '(1, 2))) :: Either (Nat, Nat) b
-- = 'Left '(1, 2)

-- >>>:kind! Evaltf (Mapt Snd ('Right '(1, 2)))
-- Evaltf (Mapt Snd ('Right '(1, 2))) :: Either a Nat
-- = 'Right 2

-- >>>:kind! Evaltf (Mapt Snd ('Left 'False))
-- Evaltf (Mapt Snd ('Left 'False)) :: Either Bool b
-- = 'Left 'False

-- >>>:kind! Evaltf (Mapt Snd ('Right '( 'False, 'True)))
-- Evaltf (Mapt Snd ('Right '( 'False, 'True))) :: Either a Bool
-- = 'Right 'True

data Mapt :: (a -> Exp b) -> f a -> Exp (f b)
type instance Evaltf (Mapt f '[]) = '[]
type instance Evaltf (Mapt f (a ': as)) = Evaltf (f a) ': Evaltf (Mapt f as)

type instance Evaltf (Mapt f 'Nothing)  = 'Nothing 
type instance Evaltf (Mapt f ('Just a)) = 'Just (Evaltf ( f a))

type instance Evaltf (Mapt f ('Left a) )  = 'Left a
type instance Evaltf (Mapt f ('Right b) ) = 'Right (Evaltf (f b))
