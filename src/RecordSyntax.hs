module RecordSyntax where

-- | name fields are functions in record syntax
data CustomerInfoR = CustomerInfoR
    { cfirstName   :: String
    , clastName    :: String
    , cwidgetCount :: Int
    , cbalance     :: Int  
    } deriving Show
-- ^ the type is; 
--  CustomerInfoR :: String -> String -> Int -> Int -> CustomerInfoR
-- ^ the kind is ; CustomerInfoR :: *
-- the type refers to value constructor  but kind refers to type 

-- | when creating values you use value constructor
-- and use each field name in any order but they must be complete
customerDidos :: CustomerInfoR
customerDidos = CustomerInfoR 
                { cbalance = 100
                , clastName = "Creatos"
                , cfirstName = "Didemos"
                , cwidgetCount = 20
                }