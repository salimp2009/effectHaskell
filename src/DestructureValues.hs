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

matchList :: (Eq a, Num a, Show a) => [a] -> String
matchList [1,2,3] = "one, two, there => here we go"
matchList list = show list

matchBool::Bool -> String
matchBool True = "this is so True :)"
matchBool bool = show bool

matchTuple :: (String, String) -> String
matchTuple ("hello", "world") = "Hello Demir => we never forget you bro"
matchTuple ("hello", name) = "hello yow " <> name
matchTuple (greeting, "Dido") =  greeting <> "didoki min :)"
matchTuple n =  show n

matchBool'::Bool -> String
matchBool' True = "this is so True :)"
matchBool' False = "so wrong => Falseee:)"

addValues :: Num a => [a] -> a
addValues = foldr (+) 0

addValues'::(Foldable t, Num a) => t a -> a
addValues' = sum

fibs' :: Integer -> Integer
fibs' n 
    | n == 0 = 0
    | n <= 2 = 1
    | otherwise =  fibs' (n-1) + fibs'(n-2)

-- | this infinite list use with take
-- e.g take 5 fibs = [1,1,2,3,5]
fibs :: [Integer]
fibs = 1 : 1 : zipWith (+) fibs (tail fibs)



