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
            "your book will be shipped to" 
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

