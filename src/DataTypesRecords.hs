module DataTypesRecords where


data CustomerInfo   = CustomerInfo Bool deriving Show
-- ^ this is a type   ^ this is a value constructor for the type
-- value constructor is a special function (value level) 

-- someCustomerInfo :: CustomerInfo
someCustomerInfo :: Bool -> CustomerInfo
someCustomerInfo = CustomerInfo

