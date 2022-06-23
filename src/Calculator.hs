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
parse' [] = Left "unexpected end of expression"
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
        parseBinary :: (Expr -> Expr -> Expr) -> [String] -> Either String (Expr, [String])
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

-- | testing for exercise Pretty print
-- use case;
-- myPrettyprint
-- -> return 5
-- -> salitos
myPrettyprint :: IO ()
myPrettyprint = putStr $ "return " ++ show 5 ++ "\n" ++ "salitos" ++ "\n" 

-- | use case;
-- >>> safeEval $ Lit 10 `Div` Lit 0
-- WAS Left "error"
-- NOW Left "error: Division by Zero"
safeEval :: Expr -> Either String Int
safeEval expr = 
    case expr of
        Lit num -> Right num
        Add x y -> Right $ eval' (+) x y
        Sub x y -> Right $ eval' (-) x y
        Mul x y -> Right $ eval' (*) x y
        Div x y -> case y of
                  Lit 0 -> Left "error: Division by Zero"
                  _     -> Right $ eval' div x y
        where
            eval' operator arg1 arg2 =
                operator (eval arg1)  (eval arg2)

prettyPrintC:: Expr -> String                
prettyPrintC expr =
    case expr of
      arg1 `Add` arg2 -> prettyPrintC arg1 <> " + " <> prettyPrintC' arg2
      arg1 `Sub` arg2 -> prettyPrintC arg1 <> " - " <> prettyPrintC' arg2
      arg1 `Mul` arg2 -> prettyPrintC arg1 <> " x " <> prettyPrintC' arg2
      arg1 `Div` arg2 -> prettyPrintC arg1 <> " / " <> prettyPrintC' arg2
      Lit x             -> show x
      where 
        prettyPrintC' arg = case arg of
          Lit x -> show x <> " = "  <> show (eval expr)
          arg'     -> " ( " <> prettyPrintC arg' <> " )" 

prettyPrint:: Expr -> String   
prettyPrint expr = prettyPrintC expr <> " = " <> show (eval expr)