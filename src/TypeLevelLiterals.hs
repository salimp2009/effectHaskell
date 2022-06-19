{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}


module TypeLevelLiterals where
import GHC.TypeLits 
                ( Nat 
                , KnownNat
                , Symbol
                , KnownSymbol
                , natVal 
                , symbolVal  
                )
import Data.Proxy (Proxy(..))

-- | example for type level literals 
-- Nat number => 1,2,3...
-- Pointer 4 with value of 8 has pointer value 32
-- it is used to make sure pointer value is aligned correctly
newtype Pointer (align :: Nat) = Pointer Integer 
        deriving Show

zeroPtr :: Pointer n
zeroPtr = Pointer 0

-- | use case;
-- >>> inc (Pointer 8 :: Pointer 4) 
-- Pointer 9
inc :: Pointer align -> Pointer align
inc (Pointer p) = Pointer (p + 1)

-- |  ScopedTypeVariables is needed so that we can use align type 
-- in the right side function body ; 
-- e.g; ....natVal (Proxy::Proxy align)
-- use case ;
-- >>> ptrValue (Pointer 8 :: Pointer 4)
-- 32
-- "natVal method og KnownNat type class takes a Proxy as an argument 
-- which makes a type level natural literal available to its term-level integer counterpart"
ptrValue :: forall align. KnownNat align => Pointer align -> Integer
ptrValue (Pointer p) = p * natVal (Proxy::Proxy align)

-- | since it is not guaranteed to get aligned pointer
-- it is safer to use Maybe Maybe (Pointer align)for return type
-- we check if the given value can be divided to align value without any remaining value
-- example return quotient which is pointer value / alignment and the call ptrValue to multiply back 
--- >>> ptrValue <$> (maybePointer 24 :: Maybe (Pointer 8))
-- Just 24
--- 
--- >>> ptrValue <$> (maybePointer 42 :: Maybe (Pointer 8))
-- Nothing
maybePointer :: forall align. KnownNat align => Integer -> Maybe (Pointer align)
maybePointer p 
        | remainder == 0 = Just (Pointer quotient)
        | otherwise = Nothing
    where 
        (quotient , remainder) = divMod p $ natVal (Proxy::Proxy align)


newtype SuffixedString (suffix :: Symbol) = SS String
                deriving Show

suffixed :: String -> SuffixedString suffix                
suffixed = SS

-- | use case 
-- >>> asString personId1
-- "Salitos@pamukcu"
--
-- >>> asString personId2
-- "Didokitos@pamukcu"
asString :: forall suffix. KnownSymbol suffix => SuffixedString suffix -> String
asString (SS str) = str <> "@" <> symbolVal (Proxy::Proxy suffix)

personId1 :: SuffixedString "pamukcu"
personId1 = suffixed "Salitos"

personId2 :: SuffixedString "pamukcu"
personId2 = suffixed "Didokitos" 

-- | the type can be directly applied to variable too
personId3 :: SuffixedString "pamukcu"
personId3 = suffixed "Semokitos" :: SuffixedString "pamukcu"





