-- {-# LANGUAGE RankNTypes #-}
-- {-# LANGUAGE TypeApplications #-}
-- {-# LANGUAGE KindSignatures #-}

module Calculator where

-- | Lit Int -> Literal integer
data Expr = Lit Int
          | Add Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr
          | Div Expr Expr
          deriving (Show)

-- | First version; revised to avoid eval repitition 
-- eval :: forall a. (Num a, Integral a) => Expr -> a
-- eval :: Expr -> Int
-- eval expr =
--     case expr of
--         Lit num -> num 
--         Add x y -> eval x + eval y
--         Sub x y -> eval x - eval y
--         Mul x y -> eval x * eval y
--         Div x y -> eval x `div` eval y

-- | Use;cases ;
-- eval $ Add  (Lit 1) (Sub (Lit 5) (Add (Lit 10 `Div`Lit 2) (Lit 3)))
-- -> -2 
-- eval $ Add (Lit 1) (Lit 2) -> 3
eval :: Expr -> Int
eval expr = 
    case expr of
        Lit num -> num
        Add x y -> eval' (+) x y
        Sub x y -> eval' (-) x y
        Mul x y -> eval' (*) x y
        Div x y -> eval' div x y
        where
            eval' operator arg1 arg2 =
                operator (eval arg1)  (eval arg2)

  -- ^ value constructor are normal function and can be used as infix function
  
parse :: String -> Either String Expr
parse str =
   case parse' (words str) of
      Left err        -> Left err
      Right (e, [])   -> Right e
      Right (_, rest)  -> Left $ "Found extra tokens: " <> unwords rest

parse' :: [String] -> Either String (Expr, [String])
parse' = undefined




    

                







