module GreetingFunction where

salutation :: String
salutation = "hello"

greeting :: String
person :: String

person = "salitos"
greeting =
    salutation <> " " <> person

makeGreeting :: String -> String -> String
makeGreeting saluteText salutePerson = saluteText <> " " <> salutePerson

greetPerson :: String -> String
greetPerson = makeGreeting "Hello"

makeGreeting' :: String -> String -> String
makeGreeting' salute = (<>) (salute <> " " )

makeGreeting'' :: String -> String -> String
makeGreeting'' = (<>) . ( <> " " )

-- the order of the variables in let bindings does not matter
extendedGreeting :: String -> String
extendedGreeting person =
    let joinWithNewLine a b = a <> "\n" <> b
        hello   = makeGreeting "Hello" person
        goodbye = makeGreeting "Goodbye" person
    in joinWithNewLine hello goodbye

-- example for intermediate variables using let
extendedGreeting' :: String -> String
extendedGreeting' person =
    let joinWithNewLine a b = a <> "\n" <> b
        joined = joinWithNewLine hello goodbye
        hello   = makeGreeting "Hello" person
        goodbye = makeGreeting "Goodbye" person
    in joined

-- example for nested let bindings intermediate functions & variables
extendedGreeting'' :: String -> String
extendedGreeting'' person =
    let joinWithNewLine a b = a <> "\n" <> b
        helloAndGoodbye hello goodbye =
            let hello' = makeGreeting hello person
                goodbye' = makeGreeting goodbye person
            in joinWithNewLine hello' goodbye'
    in helloAndGoodbye "Hello" "Goodbye"

