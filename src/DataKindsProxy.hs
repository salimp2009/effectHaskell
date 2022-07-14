{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

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

myuser = UserAdmin (Just (Proxy @Admin))

-- | example to use promoted Data constructor at type level
doSensitiveThings :: Proxy 'Admin -> IO ()
doSensitiveThings = print 

                        
