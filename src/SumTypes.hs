{-# LANGUAGE RankNTypes #-}

module SumTypes where

-- | using Sum types as Enums
data Direction = North | South | East | West

-- | Sum of Products
data PreferredContactMethod =   Email String 
                              | Text String
                              | Mail String String Int 
                              deriving Show

emailContact :: PreferredContactMethod
emailContact = Email "my@exampler.com"

textContact :: PreferredContactMethod
textContact = Text "+90 111 222 333"

mailContact :: PreferredContactMethod
mailContact = Mail "Route 66 -> 34, Istanbul" "Turkey" 34020 

confirmContact :: PreferredContactMethod -> String
confirmContact contact = case contact of
        Email emailaddres   -> "an email sent to you, please confirm: " <> emailaddres
        Text phoneNo        -> "a message has been sent, please confir" <> phoneNo
        Mail streetcity country zip -> 
            "your book will be shipped to " 
            <> streetcity <> "\n"
            <> country <> "\n"
            <> show zip <> "\n"

-- | in scenarios where you dont care about field values
-- then a curly brace can be used instead actual value
-- looks like record wildcards but no extension needed
confirmContact' :: PreferredContactMethod -> String
confirmContact' contact = case contact of
     Mail {}  -> "your book is shipped!"
     Email {} -> "an email has been to you!"
     Text {}  -> "a message has been sent to your phone!"    

contactForUser :: String -> PreferredContactMethod
contactForUser username = case username of
        "Salitos" -> Email "my@example.com"
        "Didemos" -> Text "90 111 222 333"
        "Demiros" -> Mail "Route 66" "Suite 777 CA" 90025
        name -> Email $ name <> "@someexample.com"

data StringOrNumber = S String | N Int deriving Show

stringAndNumbers ::  [StringOrNumber]
stringAndNumbers = [ S "this is a tricky list"
                   , N 2
                   , S "different type pf value"     
                   ]
-- | example Sum Products
-- Dont use this because potential runtime error
-- if trying to access record field that does exist one of the types
-- e.g salary (Customer{..})
data Person = Customer 
        { name    :: String
        , pbalance :: Int
        }                   
        | Employee
        { name        :: String
        , managerName :: String
        , salary      :: Int
        }

salitos :: Person
salitos = Employee { name = "Salim"
                   , managerName = "Semos"
                   , salary = 100
                   }

didemoss :: Person
didemoss = Customer { name = "Didoss" 
                    , pbalance = 20 
                    }   
                    
-- | if Sum Products needed ; create each Product type seperately
-- then use it in a Sum type as a type 
-- but there is still potential error if any of access fields are not
-- defined for any of the constructors then we will Exception error again !!
-- best way to return a Maybe so if there is no access field 
-- return 'Nothing' otherwise return 'Just value'

data CustomerInfor = CustomerInfor
        { customerName    :: String
        , customerBalance :: Int
        } deriving Show 

data EmployeeInfor = EmployeeInfor
        { employeeName    :: String
        , employeeManager :: String
        , employeeSalary  :: Int
        } deriving Show

data Person' = Customer' CustomerInfor | Employee' EmployeeInfor  deriving Show

semoki :: Person'
semoki = Customer' $ CustomerInfor 
                 { customerName = "Semoki" 
                 , customerBalance = 10
                 }  

-- | to access the underlying record fields we need to write 
-- functions to extract 
demiross :: Person'
demiross = Employee' $ EmployeeInfor
                       { employeeName    = "Demiros"
                       , employeeManager = "DidokiBoss"
                       , employeeSalary  = 1000
                       } 

getPersonName :: Person' -> String
getPersonName person = 
        case person of 
                Customer' customer -> customerName customer
                Employee' employee -> employeeName employee

-- | since there is employeeManager for a customer we have a problem of Exception again
-- better soln to return a Maybe so we can return Nothing in those cases
getPersonManager :: Person' -> String
getPersonManager person =
        case person of
                Employee' employee -> employeeManager employee
                Customer' customer -> undefined

-- | this is safer if the record field does not apply to one of the types
-- in the Sum type it returns Nothing other Just @String
getPersonManager' :: Person' -> Maybe String
getPersonManager' person =
        case person of
                Employee' employee -> Just $ employeeManager employee
                Customer' _customer -> Nothing 

getPersonBalance' :: Person' -> Maybe Int
getPersonBalance' person =
        case person of
                Customer' customer -> Just $ customerBalance customer
                Employee' _employee -> Nothing

getPersonSalary' :: Person' -> Maybe Int
getPersonSalary' person =
        case person of
                Employee'  employee -> Just $ employeeSalary employee
                Customer' customer  -> Nothing

mymaybeToList :: Maybe a -> [a]               
mymaybeToList Nothing    =  []
mymaybeToList (Just val) =  [val]

myEitherToMaybe :: Either b a -> Maybe a
myEitherToMaybe e =
        case e of 
                Left  _   -> Nothing
                Right val -> Just val

handleMissingRight :: forall a. Either String (Maybe a) -> Either String a
handleMissingRight  e =
        case e of
                Left err -> Left err
                Right Nothing    -> Left "Missing right value"
                Right (Just val) -> Right val


