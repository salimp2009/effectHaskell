module ParserExamples where

import Data.List (splitAt)

-- | data type that holds a function as a type
-- data StringParser = StringParser (String -> (String , String))

-- | same as above except we turn it to record type
-- data StringParser = StringParser { runStringParser :: String -> (String , String)}

-- | revised to newtype as recommended by HLS
newtype StringParser = StringParser { runStringParser :: String -> (String , String)}

takeCharacters:: Int -> [a] -> ([a], [a])
takeCharacters = splitAt

takeCharacters':: Int -> (String -> (String, String))
takeCharacters' numCharacters = stringParser
        where
            stringParser :: String -> (String , String)
            stringParser = splitAt numCharacters

takeCharacters'':: Int -> (String -> (String, String))
takeCharacters'' numCharacters = stringParser
        where
            stringParser :: String -> (String , String)
            stringParser = \inputString ->
                splitAt numCharacters inputString

takeCharacters''' :: Int -> StringParser
takeCharacters''' numCharacters = StringParser stringParser
        where
            stringParser :: String -> (String , String)
            stringParser = \inputString ->
                splitAt numCharacters inputString

-- | this function takes an Int and gives you a type StringParser 
-- which has a function inside that has a type String -> (String, String)
-- it is a record type ; to be able to use that function
-- you have to use runStringParser and give a String so it gives (String, String) 
-- |use case ;
-- newParse = takeCharactersR 3
-- runStringParser newParse  "dfs12345"  -> ("dfs","12345")
-- runStringParser newParse $ "dfs12345" -> ("dfs","12345")
-- newParse2 = takeCharactersR  
-- >runStringParser (newParse2 4) $ "123dsfsfsd" -> ("123d","sfsfsd")
takeCharactersR :: Int -> StringParser
takeCharactersR numCharacters = StringParser $ \inputString ->
    splitAt numCharacters inputString

-- | similar but compose functions
-- just apply the number and String to get the result
-- use case; 
-- newStrparser 3 "123dsfsfsd" -> ("123","dsfsfsd")
newStrparser :: Int -> String -> (String, String)
newStrparser = runStringParser.takeCharactersR

-- | seperates the given list at the condition is True
-- and return a list of tuples
-- here we stop at the first space
-- second list will contain everything after the space there we use tail
-- use case ;
-- runStringParser getNextWord "salim kalitos pasos" ->("salim","kalitos pasos")
-- note if we don use the tail in second list of the tuple then we will have space at front
-- e.g: -> ("salim"," kalitos pasos")  
getNextWord :: StringParser
getNextWord = StringParser $ \someString ->
        case break (==' ') someString of
            (nextWord, "")   -> (nextWord, "")
            (nextWord, rest) -> (nextWord, tail rest)

-- | use case; 
-- getNextWord' "sdfs dsfsdf sdfsdf sdfsf" -> ("sdfs","dsfsdf sdfsdf sdfsf")
getNextWord' :: String -> (String, String)
getNextWord' = runStringParser.StringParser $ \someString ->
        case break (==' ') someString of
            (nextWord, "")   -> (nextWord, "")
            (nextWord, rest) -> (nextWord, tail rest)

-- | use case with other helper functions
-- thirdWord = combineParsers getNextWord $ combineParsers getNextWord getNextWord
-- parseString thirdWord "sdfsd dfsfd 3rdword dsfsdf dfsgsfgsd" -> "3rdword"
combineParsers :: StringParser -> StringParser -> StringParser
combineParsers fstParser sndParser = StringParser $ \someString ->
        let (_firstPart, firstResult) = runStringParser fstParser someString
        in  runStringParser sndParser firstResult

-- | use case;
-- runStringParser combinedParsers "sadas sdsd sda asda" -> ("sdsd","sda asda")
combinedParsers :: StringParser
combinedParsers = combineParsers getNextWord getNextWord

-- | use case;
-- getResultCombinedParsers "sadas sdsd sda asda" -> ("sdsd","sda asda")
getResultCombinedParsers :: String -> (String, String)
getResultCombinedParsers = runStringParser combinedParsers

getNextWordAfterTenLetters :: StringParser
getNextWordAfterTenLetters =  combineParsers (takeCharactersR 10) getNextWord

-- | use case with ParseString
-- parseString tenLettersAfterTheFirstWord  "abcdefgyta 012345Haskell" -> "012345Hask"
tenLettersAfterTheFirstWord :: StringParser
tenLettersAfterTheFirstWord = combineParsers getNextWord (takeCharactersR 10)

-- | Helper function to use / parse StringParser 
-- use case;
-- parseString (takeCharactersR 5) "hello, haskell" -> "hello,"
parseString :: StringParser -> String -> String
parseString parser inputString =
    fst $ runStringParser parser inputString              