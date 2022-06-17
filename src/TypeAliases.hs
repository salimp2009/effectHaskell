module TypeAliases where

-- | Example of how not to use Type aliases
type Meters  = Double
type Seconds = Double
type MetersPerSecond = Double

velocity :: Meters -> Seconds -> MetersPerSecond
velocity meters seconds = meters / seconds

gravity :: Double
gravity = 
    let 
        meters = 9.8 :: Meters
        seconds = 1.0 :: Seconds
    in velocity meters seconds 

-- | Right ways to use type aliases
-- type alias is used to apply a type to a partially applied type
-- e.g : String' applies Char to NewList and creates NewList Char
-- data NewList a = EmptyNew | NewList a
-- type String' = NewList Char

-- | Sample use of type aliases
-- types, functions are not implemented; only for show purpose
-- data AppError = AppError 
--     { errorMessage :: String
--     , errorContext :: [String]
--     , errorWrapped :: Maybe AppError
--     }

-- data Order 
-- parseUserOrder :: String -> Either AppError Order
-- parseUserOrder = undefined

-- data Invoice   
-- generateInvoice :: Order -> Either AppError Invoice
-- generateInvoice = undefined 

-- data Widget
-- updateInventory :: [Order] -> [(Widget, Int)] -> Either AppError [(Widget, Int)]
-- updateInventory = undefined 
-- -- ^ too much repetition of Either AppError a; 

-- -- | Type alias can be used to make it easier if used on a bigger code base
-- type AppValue a = Either AppError a
-- parseUserOrder' :: String -> AppValue Order
-- parseUserOrder' = undefined

-- generateInvoice' :: Order -> AppValue Invoice
-- generateInvoice' = undefined 

-- updateInventory' :: [Order] -> [(Widget, Int)] -> AppValue [(Widget, Int)]
-- updateInventory' = undefined 