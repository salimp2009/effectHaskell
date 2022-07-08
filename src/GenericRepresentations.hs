-- {-# LANGUAGE DefaultSignatures #-}
-- {-# LANGUAGE DeriveAnyClass #-}
-- {-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
-- {-# LANGUAGE StandaloneDeriving #-}
-- {-# LANGUAGE TypeOperators #-}

module GenericRepresentations where

import GHC.Generics

-- | Canoical representations are sum-of-products
-- Maybe a can represented using Either () a
-- proof using isomorphism
toCanoical :: Maybe a -> Either () a
toCanoical Nothing = Left ()
toCanoical (Just x) = Right x

fromCanoical :: Either () a -> Maybe a
fromCanoical (Left ()) = Nothing
fromCanoical (Right x) = Just x

-- | Bool generic representation
-- True and False has no argument therefore isomorphic to ()
-- Canoical representation of Bool is Either () ()
-- >>>:kind! Rep Bool
-- Rep Bool :: * -> *
-- = D1
--     ('MetaData "Bool" "GHC.Types" "ghc-prim" 'False)
--     (C1 ('MetaCons "False" 'PrefixI 'False) U1
--      :+: C1 ('MetaCons "True" 'PrefixI 'False) U1)

-- >>> from True
-- M1 {unM1 = R1 (M1 {unM1 = U1})}

toBoolCanoical :: Bool -> Either () ()
toBoolCanoical False = Left ()
toBoolCanoical True = Right ()

fromCanoicalBool :: Either () () -> Bool
fromCanoicalBool (Left ()) = False
fromCanoicalBool (Right ()) = True


