{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
--{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
--{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
--{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
--{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module CustomTypeErrors where

import Data.Kind (Constraint, Type)
import Data.Proxy (Proxy (..))
import qualified Data.Vector as V
import Fcf
import GHC.OverloadedLabels (IsLabel (..))
import GHC.TypeLits 
import Unsafe.Coerce (unsafeCoerce)

-- | TypeError is usually used as a constraint in an
-- instance context, or as the result of a type family
-- type family TypeError (a :: ErrorMessage) :: b where
-- >>>1 True
-- Attempting to use a type as a function
-- in the type `Bool -> t`
-- Maybe you forgot to specify a function?
instance (TypeError 
            (Text "Attempting to use a type as a function"
             :$$: Text "in the type `" 
             :<>: ShowType (a -> b)     
             :<>: Text "`"
             :$$: Text "Maybe you forgot to specify a function?"
             )
      ) => Num (a -> b) where
  (*) = undefined
  (+) = undefined
  abs = undefined
  signum = undefined
  fromInteger = undefined
  negate = undefined

-- |  four means of constructing ERRORMESSAGEs are available;
{-
   'Text (of kind SYMBOL → ERRORMESSAGE.) Emits the
  symbol verbatim. Note that this is not
  Data.Text.Text.
   'ShowType (of kind K → ERRORMESSAGE.) Prints the name
  of the given type.
   '(:<>:) (of kind ERRORMESSAGE → ERRORMESSAGE →
  ERRORMESSAGE.) Concatenate two ERRORMESSAGEs
  side-by-side.
   '(:$$:) (of kind ERRORMESSAGE → ERRORMESSAGE →
  ERRORMESSAGE.) Append one ERRORMESSAGE vertically
  atop another.
-}

{-
  "When evaluating 1 True, Haskell matches the instance
  head of Num (a -> b), and then attempts to solve its
  context. Recall that whenever GHC sees a TypeError, it
  fails with the given message. We can use this principle to
  emit a friendlier type error when using prj incorrectly."
-}
-- | this is specific for OpenSums project 
-- and used in proj2 function in OpenSums module 
type FriendlyFindElem :: (k-> Type) -> k ->[k] -> Exp Nat
type family FriendlyFindElem f t ts where
    FriendlyFindElem f t ts = 
        FromMaybe 
        (TypeError
        ('Text "Attempt to call `friendlyProj' to produce a `"
        ':<>: 'ShowType (f t) ':<>: Text "'."
        ':$$: 'Text "But OpenSum can only contain of:"
        ':$$: 'Text " " ':<>: 'ShowType ts
        )
        ) =<< FindIndex (TyEq t) ts

{- ^ 
   "Notice that FriendlyFindElem is defined as a type family,
   rather than a type synonym as FCFs usually are. This is to
   delay the expansion of the type error so GHC doesn’t emit
   the error immediately. We now attempt to find t in ts, and
   use FromMaybe to emit a type error in the case that we didn’t
   find it"
-} 

-- >>>:kind! ShowList [1, 2, 3]
-- ShowList [1, 2, 3] :: ErrorMessage
-- = ('ShowType 1 ':<>: 'Text ", ")
--   ':<>: (('ShowType 2 ':<>: 'Text ", ") ':<>: 'ShowType 3)
type family ShowList (ts::[k]) where
  ShowList '[] = Text ""
  ShowList (t ': '[]) = ShowType t
  ShowList (t ': ts) = ShowType t ':<>: Text ", " ':<>: ShowList ts 


