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
       | otherwise      = go (i + 2)

-- | this is not efficient see version 2 in TypeTHPrimesTH module for a better impl.
primesUpTo :: Integer -> [Integer]
primesUpTo n = go 2 
  where 
    go i
      | i > n     = []
      | isPrime i = i : go (i + 1)
      | otherwise = go (i + 1)

      

