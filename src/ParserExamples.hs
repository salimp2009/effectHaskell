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
            
takeCharactersR :: Int -> StringParser
takeCharactersR numCharacters = StringParser $ \inputString -> 
    splitAt numCharacters inputString   
               