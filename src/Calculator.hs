-- {-# LANGUAGE RankNTypes #-}
-- {-# LANGUAGE TypeApplications #-}

module Calculator where

-- | Lit Int -> Literal integer
data Expr = Lit Int
          | Add Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr
          | Div Expr Expr
          deriving (Show)

-- eval :: forall a. (Num a, Integral a) => Expr -> a
eval expr =
    case expr of
        Lit num -> num 
        Add x y -> eval x + eval y
        Sub x y -> eval x - eval y
        Mul x y -> eval x * eval y
        Div x y -> eval x `div` eval y
