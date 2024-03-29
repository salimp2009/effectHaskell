{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module MonadBasics where

import ApplicativeBasic (List(..)
                        , concatList
                        , toList
                        , fromList
                        )

import Text.Read (readMaybe)
import Data.Functor ( (<&>) )
import Data.String (IsString(..))

-- | use case;
-- >>> readMaybe "10" >>= half
-- Just 5
half :: forall a. Integral a => a -> Maybe a
half val 
        | even val  = Just (val `div` 2)
        | otherwise = Nothing
-- | use case;
-- >>> readMaybe "10" >>= half >>= bound (2, 20)
-- Just 5
-- >>> readMaybe "10" >>= half >>= bound (6, 20)
-- Nothing
-- >>> readMaybe "11" >>= half >>= bound (2, 20)
-- Nothing
bound :: (Int, Int) -> Int -> Maybe Int
bound (min, max) val
            | (val >= min) && (val <= max) = Just val
            | otherwise = Nothing

result1 :: Maybe Int
result1 = readMaybe @Int "11" >>= bound (0,20) >>= half . succ  

-- | (<&>) has type :: f a -> (a -> b) -> f b
-- 
result2 :: Maybe Int
result2 = readMaybe "11" >>= bound (0,20) <&> succ >>= half

-- | having succ compose with return makes it a Monad and gives its result
-- to following function half 
result3 :: Maybe Int
result3 = readMaybe "11" >>= bound (0,20) >>= return . succ >>= half

instance Monad List where
    return a = List a Empty
    Empty >>= f     = Empty
    List a as >>= f = f a `concatList` (as >>= f) 
        
-- | extension used for the List example
--    {-# LANGUAGE OverloadedStrings #-}
-- ^ this extension used to define values of types other than String
-- generally used bytestrings and Text ; very common / usefull
-- here used to define a custom string type using our List type
--    {-# LANGUAGE FlexibleInstances #-}
-- ^ this extension used to remove some restriction to create instances
-- of type classes for one specific instance; it is safe to use if needed

-- | using extension and below instance and using toList
-- and Show instance for our List us allow this ;
-- >>> "hello haskell" :: List Char
-- "hello haskell"
-- >>> listChar = ("foo"::List Char) `concatList` ("bar"::List Char)
-- >>> listChar
-- >>>:t listChar
-- "foobar"
-- listChar :: List Char
instance IsString (List Char) where
        fromString = toList

replicateL :: Int -> a -> List a
replicateL 0 _ = Empty
replicateL n val = 
        let tail = replicateL (pred @Int n) val
        in List val tail

type StringL = List Char

-- | use case;
-- >>> wordsL ("hello haskell")
-- ["hello","haskell"]
-- >>> wordsL ("hello haskell" :: StringL)
-- ["hello","haskell"]
wordsL :: StringL -> List StringL
wordsL = toList . (toList <$>) . words . fromList

unwordsL :: List StringL -> StringL
unwordsL = toList . unwords . fromList . (fromList <$>)

-- | creating List nested List of nums; each num is a List
-- use case;
-- >>> nestedListnums 
-- [[1],[2],[3],[4],[5]]
-- >>> nestedListnums >>= id
-- [1,2,3,4,5]
nestedListnums :: (Num a, Enum a) => List (List a)
nestedListnums = toList $ map (return @List) [1..5] 

-- | use case;
-- >>> nestedLists ["sal", "id", "haskel"] 
-- [["sal"],["id"],["haskel"]]
-- >>>:t it
-- it :: IsString a => List (List a)
-- >>> it >>=id
-- ["sal","id","haskel"]

-- >>> nestedLists ["sal", "id", "haskel"] :: List (List (List Char)) 
-- [["sal"],["id"],["haskel"]]
-- >>> it >>=id >>= id
-- "salidhaskel"
-- >>>:t it
-- it :: List Char
nestedLists :: [a] -> List (List a)
nestedLists= toList . map (return @List)

-- | this is actually join in Monads
myconcatM :: Monad m => m (m a) -> m a
myconcatM x = x >>= id

-- >>> pure "hello haskell" >>= wordsL 
-- ["hello","haskell"]

-- >>> pure "hello haskell" >>= wordsL >>= replicateL 3
-- ["hello","hello","hello","haskell","haskell","haskell"]

-- >>> pure "hello haskell" >>= wordsL >>= replicateL 3 >>= id
-- "hellohellohellohaskellhaskellhaskell"

-- >>> unwordsL $ pure "hello haskell" >>= wordsL
-- "hello haskell"

-- >>> pure "hello haskell" >>= wordsL >>= unwordsL


-- >>> unwordsL $ pure "hello haskell" >>= wordsL >>= const Empty
-- ""
