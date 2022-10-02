module TypedTHPrimes where

isPrime :: Integer -> Bool
isPrime n
    | n <= 1 = False
    | n == 2 = True
    | even n = False
    | otherwise = go 3
    where
      go i 
       | i >= n         = True
       | n `mod` i == 0 = False
       | otherwise      = go (n+2)


isPrimeUpTo :: Integer -> [Integer]
isPrimeUpTo n = go 2 
  where 
    go i
      | i > n     = []
      | isPrime i = i : go (i + 1)
      | otherwise = go (i + 1)

