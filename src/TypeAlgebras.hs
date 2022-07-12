{-# LANGUAGE DerivingStrategies #-}
-- {-# LANGUAGE DerivingVia #-}
-- {-# LANGUAGE GeneralizedNewtypeDeriving #-}
module TypeAlgebras where

import Data.Void (Void, absurd)
import Control.Monad.RWS (Alt)


-- | proof a x 1 = a by showing isomorphism
-- between (a, ()) and a
-- unit type '()' is monoidal identity for product types
-- unit type is resembled by 1 in math since it has only one inhabitant
productUnitto :: a -> (a, ()) 
productUnitto a = (a, ())

productUnitFrom :: (a, ()) -> a
productUnitFrom (a, ()) = a

-- | proof a :+: 0 =  a
-- a + 0 = a
-- Either a Void 
-- Void is resembled as 0 since no inhabitant 
-- Void is the monoidal unit for sum types
sumUnitto :: Either a Void -> a
sumUnitto (Left a) = a
sumUnitto (Right v) = absurd v

sumUnitFrom :: a -> Either a Void
sumUnitFrom  = Left 

-- | naive implementation of board game with nine spaces
-- a x a... x a = a^9 ~ a ^ (3 x 3)
data TicTacToe a = TicTacToe
    { topLeft   :: a
    , topCenter :: a
    , topRight  :: a
    , midLeft   :: a
    , midCenter :: a
    , midRight  :: a
    , botLeft   :: a
    , botCenter :: a
    , botRight  :: a
    } deriving stock Show

-- | too much work to just fill an empty board
-- use | TicTacToe | = a^9 ~ a^3x3
-- TicTacToe is isomorphic to; (Three, Three) -> a
-- or curried form; Three -> Three -> a 
-- Three can be any data with 3 inhabitants; e.g a sum type with 3 constructors
emptyboard :: TicTacToe (Maybe Bool)    
emptyboard = TicTacToe
    Nothing Nothing Nothing
    Nothing Nothing Nothing
    Nothing Nothing Nothing

data Three = One | Two | Three
    deriving (Eq, Ord, Enum, Bounded, Show)

newtype TicTacToe2 a = TicTacToe2 
        { board :: Three -> Three -> a } 

-- | use case;  
-- restructuring the TicTacToe using its isomorphic rep
-- we achieved a simpler interface      
-- >>> board  emptyboard2 One Two    
-- Nothing
emptyboard2 :: TicTacToe2 (Maybe Bool)        
emptyboard2 =
    TicTacToe2 (\_ _ -> Nothing)
-- >>> board emptyboard3 One Two 
-- Nothing
emptyboard3 :: TicTacToe2 (Maybe Bool)        
emptyboard3 =
    TicTacToe2 $ const $ const Nothing   