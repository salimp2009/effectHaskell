module ConditionalBRanch where

printSmallNumber :: (Ord a, Num a, Show a) => a -> IO ()
printSmallNumber num =
    if num < 10
    then print num
    else print "number is too big!"


printSmallNumber' :: (Ord p, Num p, Show p) => p -> IO ()
printSmallNumber' num =
    let msg = if num < 10 
            then show num
            else "the number is too big:" <> show num
    in print msg

guardSize :: (Ord a, Num a, Show a) => a -> String
guardSize num
    | num < 3 = "that is a small num: " <> show num
    | num < 10 = "that is a medium :   " <> show num
    | num < 100 = "that is a big one :   " <> show num
    | num < 1000 = "that is a medium :   " <> show num
    | otherwise =  "that is way biiggg :)" <> show num

fizzBuzzFor :: (Integral a, Show a) => a -> String
fizzBuzzFor number 
    | 0 == number `rem` 15 = "fizzbuzz"
    | 0 == number `rem` 5 = "buzz"
    | 0 == number `rem` 3 = "fizz"
    | otherwise = show number

naiveFizzBuzz :: (Integral a, Show a) => a -> a -> String -> String
naiveFizzBuzz fzBzCount curNum fzBzString 
    | curNum > fzBzCount = fzBzString
    | otherwise =
        let nextfzBzString = fzBzString <> fizzBuzzFor curNum <> " "
            nextcurNum = curNum + 1
        in naiveFizzBuzz fzBzCount nextcurNum  nextfzBzString       
    

naiveFizzBuzz' :: (Integral a, Show a) => a -> a -> String -> String
naiveFizzBuzz' fzBzCount = gofzBz fzBzCount 
        where  gofzBz fzBzCount' curNum fzBzString 
                    | curNum > fzBzCount' = fzBzString 
                    | otherwise =
                        let nextString = fzBzString <> fizzBuzzFor curNum <> " "
                            nextNumber = curNum + 1
                        in gofzBz  fzBzCount nextNumber nextString

naiveFizzBuzz'' :: (Integral a, Show a) => a -> a -> String -> String
naiveFizzBuzz'' = gofzBz
        where  gofzBz fzBzCount' curNum fzBzString 
                    | curNum > fzBzCount' = fzBzString 
                    | otherwise =
                        let nextString = fzBzString <> fizzBuzzFor curNum <> " "
                            nextNumber = curNum + 1
                        in gofzBz  fzBzCount' nextNumber nextString

fizzes :: [String]
fizzes = cycle ["", "", "Fizz"]

buzzes::[String]
buzzes = cycle ["", "", "", "", "Buzz"]

-- this works if used with take n 
fizzBuzzes::[String]
fizzBuzzes = zipWith (<>) fizzes buzzes

--this works; the original implementation gave error
-- for illegal use of lamba here;
-- where pick = \case "" -> show
--                    s  -> const s
fizzBuzzBetter:: Int -> [String]
fizzBuzzBetter n = take n $ zipWith pick fizzBuzzes [1..]
            where pick a b 
                        | a == "" = show  b 
                        | otherwise = a

myCurry::((a,b)->c) -> a ->b ->c
myCurry f x y = f (x,y)

myunCurry :: (t1 -> t2 -> t3) -> (t1, t2) -> t3
myunCurry f (x,y) = f x y

myLift :: (t -> t -> t) -> [t] -> [t] -> [t]
myLift f [] [] = []
myLift f (x:xs) [] = x:xs
myLift f [] (y:ys)  = y:ys
myLift f (x:xs) (y:ys) = f x y : myLift f xs ys


myListos :: [[Integer]]
myListos = [1,2]:[]

myListos' :: [Integer]
myListos' = 1:[1,2]

myListos'' :: [[Integer]]
myListos'' = [1,2] :[[2,3]]

myListos''' :: [Integer]
myListos''' = 1:2:[]


