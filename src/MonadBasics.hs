{-# LANGUAGE RankNTypes #-}
module MonadBasics where

import Text.Read (readMaybe)

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
