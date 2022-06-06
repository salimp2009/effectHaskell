{-# LANGUAGE RecordWildCards #-}

module RecordSyntax where

-- | name fields are functions in record syntax
-- added 'c' at the beginning of the record fields
-- since we used the same name in another module
-- as a free function for CustomerInfo''
-- this problem can be solved with Generics in GHC
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
-- see below a simpler method using WildCards
customerDidos :: CustomerInfoR
customerDidos = CustomerInfoR 
                { cbalance = 100
                , clastName = "Creatos"
                , cfirstName = "Didemos"
                , cwidgetCount = 20
                }
-- | no partial application of the field names when creating; all of them needs to be set  
customerFactory :: String -> String -> CustomerInfoR
customerFactory fname lname = CustomerInfoR
                            { cbalance = 0
                            , cwidgetCount = 10
                            , cfirstName = fname
                            , clastName = lname
                            }
-- record field constructor are composable 
-- since they are ordinary functions
totalWidgetCount :: [CustomerInfoR] -> Int
totalWidgetCount = sum . map cwidgetCount

-- updating an existing record can be done
-- by applying only the updated record fields only
-- but updating does not change existing record 
-- it creates a new one and original stays the same
emptyCart :: CustomerInfoR -> CustomerInfoR
emptyCart customer = customer { cbalance = 0
                              , cwidgetCount = 0
                              }
-- | using RecordWildCard extension 
-- this enables us to use record fields as record variables
-- it can also be used for creating new records
showCustomerR :: CustomerInfoR -> String
showCustomerR CustomerInfoR{..} =
    cfirstName <> " " <> clastName <>" " <>
    show cwidgetCount <> " " <> show cbalance

-- | creating new value of record using using RecordWildCard extension
-- using field constructors as local variable to set value
-- note the local variable name has to match
customerSemo :: CustomerInfoR
customerSemo = 
    let cfirstName  = "Semokitos"
        clastName   = "TokitosMutos"
        cwidgetCount = 10
        cbalance = 2200
    in CustomerInfoR {..}

-- | function parameters can be use record field constructor as 
-- long as the names match
customerFactory' :: String -> String -> CustomerInfoR
customerFactory' cfirstName clastName =
    let  cwidgetCount = 10
         cbalance = 0
    in CustomerInfoR {..}
