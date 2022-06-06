module DataTypesRecords where

-- See RecordSyntax file for Records !!

newtype CustomerInfo  = CustomerInfo Bool deriving Show
-- ^ this is a type   ^ this is a value constructor for the type
-- value constructor is a special function (value level) 

-- someCustomerInfo :: CustomerInfo
someCustomerInfo :: Bool -> CustomerInfo
someCustomerInfo = CustomerInfo

data CustomerInfo'   = CustomerInfo' Bool Bool deriving Show

data CustomerInfo''   = CustomerInfo'' String String Int Int deriving Show

customerSalitos :: CustomerInfo''
customerSalitos = CustomerInfo'' "Salitos" "Creatos" 10 100

showCustomer :: CustomerInfo'' -> String
showCustomer (CustomerInfo'' first last count balance) =
            let fullName = first <> " " <> last 
                name = "name: " <> fullName
                count' = "count: " <> show count
                balance' = "balance: " <> show balance
            in  name <> " " <> count' <> " " <> balance'

-- | not an efficient way of doing the updates or changes; 
-- better create functions or records and/orlenses
applyDiscount :: CustomerInfo'' -> CustomerInfo''
applyDiscount customer =
    case customer of 
        (CustomerInfo'' "Salitos" "Creatos" count balance) ->
            CustomerInfo'' "Salitos" "Creatos" count (balance `div` 4)
        (CustomerInfo'' "Didemos" "Creatos" count balance) ->
            CustomerInfo'' "Didemos" "Creatos" count (balance `div` 2)
        otherCustomer -> otherCustomer

-- | None of the below are neccessary in practice use records with fiels names
firstName :: CustomerInfo'' -> String
firstName (CustomerInfo'' name _ _ _ ) = name

lastName :: CustomerInfo'' -> String
lastName (CustomerInfo'' _ name _ _ ) = name

widgetCount :: CustomerInfo'' -> Int
widgetCount (CustomerInfo'' _ _ count _ ) = count

balance :: CustomerInfo'' -> Int
balance (CustomerInfo'' _ _ _ count ) = count

updateFirstName :: CustomerInfo'' -> String -> CustomerInfo''
updateFirstName (CustomerInfo'' _ lastname count balance) firstName =
    CustomerInfo'' firstName lastname count balance

updateLastName :: CustomerInfo'' -> String -> CustomerInfo''
updateLastName (CustomerInfo'' firstname _ count balance) lastName =
    CustomerInfo'' firstname lastName count balance

