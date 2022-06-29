{-# LANGUAGE RankNTypes #-}

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

-- | this is just for exercise purpose to define
-- '(**)' and 'unit' using Applicative 'pure' and applicative
-- '<*>' and fmap (<$>)
class (Functor f) => MyMonoidal f where
    unit ::  f ()
    (**) :: f a -> f b -> f(a,b)
-- ^ in GHC this is shown as >*< @Control.Invertible.Monoidal@

-- | just to show they can be implemented via Applicative 
-- need to alter the name  ;typically you would make an instance of a type 
-- and define unit    
unit' :: Applicative f => f ()    
unit' = pure ()

-- | just to show they can be implemented via Applicative 
-- similar to '(**)' or '(>*<)' in @Control.Invertible.Monoidal@
(>**<) :: Applicative f => f a1 -> f a2 -> f (a1, a2)
(>**<) mx my =  (,) <$> mx <*> my