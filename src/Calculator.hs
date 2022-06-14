-- {-# LANGUAGE RankNTypes #-}
-- {-# LANGUAGE TypeApplications #-}
-- {-# LANGUAGE KindSignatures #-}

module Calculator where

import Text.Read (readEither)

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

  -- ^ value constructors are normal functions and can be used as infix function
  
parse :: String -> Either String Expr
parse str =
   case parse' (words str) of
      Left err        -> Left err
      Right (e, [])   -> Right e
      Right (_, rest)  -> Left $ "Found extra tokens: " <> unwords rest

parse' :: [String] -> Either String (Expr, [String])
parse' [] = Left "unexpected end of epxression"
parse' (token : rest) = 
    case token of
      "+" -> parseBinary Add rest
      "-" -> parseBinary Sub rest
      "*" -> parseBinary Mul rest
      "/" -> parseBinary Div rest
      lit ->
        case readEither lit of
            Left err    -> Left err
            Right lit'  -> Right (Lit lit', rest)
      where
        parseBinary exprConstructor args =
          case parse' args of 
            Left err -> Left err
            Right (firstArg, rest') ->
              case parse' rest' of
                Left err -> Left err
                Right (secondArg, rest'') ->
                  Right (exprConstructor firstArg secondArg , rest'') 

  -- | use cases ; 
--  run " + 3 5"      -> "The Answer is : 8"
--  run " + 3 - 8 5"  -> "The Answer is : 6"
--  run "-10 5"       -> "Found extra tokens: 5"
--  run "- -10 5"     -> "The Answer is : -15"
--  run "- 15 + 1 * 2 / 8 4" -> "The Answer is : 10"
run :: String -> String
run expr =
  case parse expr of
    Left err -> err
    Right expr' -> 
        let answer = show $ eval expr'
        in "The Answer is : " <> answer
        






    

                







