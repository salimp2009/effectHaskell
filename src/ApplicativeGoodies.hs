module ApplicativeGoodies where

import StateContext ( WithCounter
                    , validatePerson
                    , validateName
                    , validateAge
                    , Person(..)
                    )

import Data.Char
import Text.ParserCombinators.ReadP

-- | validate age but discard value
-- if validate age fails returns @Nothing@ then whole result fails return @Nothing@
-- use case ;
-- >>> boardingPass "salitos" (-10)
-- Nothing
-- >>> boardingPass "salitos" 50
-- Just "SALITOS"
boardingPass :: String -> Int -> Maybe [Char]
boardingPass name age = map toUpper <$> validateName name <* validateAge age

-- | use case;
-- >>> boardingPass' "salitos" (-10)
-- Nothing
-- >>> boardingPass' "salitos" 50
-- Just "SALITOS"
boardingPass' :: String -> Int -> Maybe [Char]
boardingPass' name age = map toUpper <$ validateAge age <*> validateName name

ageTwentyPerson :: String -> Maybe Person
ageTwentyPerson name = (`Person` 20) <$> validateName name

ageTwentyPerson' :: String -> Maybe Person
ageTwentyPerson' name = flip Person 20 <$> validateName name

ageTwentyPerson'' :: String -> Maybe Person
ageTwentyPerson'' name = Person <$> validateName name <*> pure 20

-- | example from GHC 
-- not sure how it works :)
parser :: ReadP String
parser = string "hello yow " *> munch1 isAlpha <* eof

parsertoState :: [(String, String)]
parsertoState = readP_to_S parser "hello yow Salitos"