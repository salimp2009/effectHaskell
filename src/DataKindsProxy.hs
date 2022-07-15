{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoStarIsType #-}

module DataKindsProxy where
import Data.Proxy (Proxy(..))
import GHC.TypeLits


-- | DataKinds extension promotes
-- type constructor UserType of king type
-- data constructor User is promototed 'User of kind UserType
-- data constructor Admin is promototed 'Admin of kind UserType
-- promoted data constructor can only be used at type level
-- Maybe 'User is not allowed; since Maybe a ; type parameter is used at term level
-- Maybe (Proxy 'Admin) is OK
data UserType = User | Admin

newtype UserAdmin = UserAdmin { userAdminToken :: Maybe (Proxy 'Admin)}

myuser :: UserAdmin
myuser = UserAdmin (Just (Proxy @Admin))

-- | example to use promoted Data constructor at type level
doSensitiveThings :: Proxy 'Admin -> IO ()
doSensitiveThings user = print "salitos"

myAdminfunct :: UserAdmin -> IO ()
myAdminfunct user = case userAdminToken user of
            Just admin -> doSensitiveThings admin
            _          -> print "not authorized!!" 

-- | DataKinds extension promotes almost all built-in type
-- Strings to Symbols, 
-- natural numbers to NAT; no negative, fractional nor floating
-- so we can use those at type level functions                        
-- >>>:k "salitos"
-- "salitos" :: Symbol

-- >>>:k AppendSymbol
-- AppendSymbol :: Symbol -> Symbol -> Symbol

-- >>>:k CmpSymbol
-- CmpSymbol :: Symbol -> Symbol -> Ordering

-- >>>:k CmpSymbol "salitos" "salitos"
-- CmpSymbol "salitos" "salitos" :: Ordering

-- | Ordering also promoted; e.g see 'EQ
-- >>>:kind! CmpSymbol "salitos" "salitos"
-- CmpSymbol "salitos" "salitos" :: Ordering
-- = 'EQ

-- >>>:kind! CmpSymbol "salitos" "didokis"
-- CmpSymbol "salitos" "didokis" :: Ordering
-- = 'GT

-- >>>:kind 5085555
-- 5085555 :: Nat

-- >>>:kind! (1 + 17)
-- (1 + 17) :: Nat
-- = 18

-- >>>:kind! (128 `Div ` 8) ^ 2
-- (128 `Div ` 8) ^ 2 :: Nat
-- = 256

-- | List constructor are also promoted with DataKinds
-- | Term level list;
-- >>>:k [Bool]
-- [Bool] :: Type

-- | Type level list 
-- >>>:k '[Bool]
-- '[Bool] :: [Type]

-- >>>:k '[ 'True ]
-- '[ 'True ] :: [Bool]

-- >>>:k '['True ]
-- parse error on input ‘'’

-- | Tuples are promoted as well
-- >>>:k '(2, "tupilotos")
-- '(2, "tupilotos") :: (Nat, Symbol)

-- >>>:k '(2, "tupilotos", '[ 'True])
-- '(2, "tupilotos", '[ 'True]) :: (Nat, Symbol, [Bool])





