module DestructureValues where

customGreeting :: String -> String
customGreeting "Salitos" = "Hello Salitos!"
customGreeting name = "Goodmorning " <> name

-- | this is to show 2nd version with "Salitos" will never be used
customGreeting' :: String -> String
customGreeting' name = "Goodmorning " <> name
customGreeting' "Salitos" = "Hello Salitos!"

matchNum :: (Eq a, Num a, Show a) => a -> String
matchNum 0 = "zero"
matchNum n = show n



