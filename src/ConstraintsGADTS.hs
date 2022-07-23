{-# LANGUAGE GADTs #-}
module ConstraintsGADTS where

-- | just stupid example to type equality 
-- '(a ~ Int)' ; type equalities have properties;
-- reflexivity—a type is always equal to itself: a ∼ a
-- symmetry—a ∼ b holds if and only if b ∼ a
-- transitivity—if we know both a ∼ b  and b ∼ c
-- GHC can infer that a ∼ c.
fiveplus :: (a ~ Int ) => a -> a
fiveplus = (+1)

-- | typical GADT
data ExprN a where
    LitInt  :: Int  -> ExprN Int
    LitBool :: Bool -> ExprN Bool
    AddN     :: ExprN Int -> ExprN Int -> ExprN Int
    Not     :: ExprN Bool -> ExprN Bool
    If      :: ExprN Bool -> ExprN a -> ExprN a -> ExprN a

-- | use case;
-- >>> evalExprN . Not $ LitBool True    
-- False

-- >>> evalExprN . Not $ LitBool True
-- False

-- >>> evalExprN . If (LitBool False) (LitInt 1). AddN (LitInt 4) $ LitInt 15
-- 19

-- >>> evalExprN . If (LitBool True) (LitInt 1). AddN (LitInt 4) $ LitInt 15
-- 1
evalExprN :: ExprN a -> a
evalExprN (LitInt i)  = i
evalExprN (LitBool b) = b
evalExprN (AddN x y)   = evalExprN x + evalExprN y
evalExprN (Not b)     = not $ evalExprN b
evalExprN (If b x y) = 
            if evalExprN b 
                then evalExprN x 
                else evalExprN y 
                
-- | GADT is sugarized version class data type with Type Equalities
data Expr_ a 
    = (a ~ Int ) => LitInt_ Int
    | (a ~ Bool) => LitBool_ Bool
    | (a ~ Int ) => Add_ (Expr_ Int) (Expr_ Int)
    | (a ~ Bool) => Not_ (Expr_ Bool)
    | If_ (Expr_ Bool)  (Expr_ a) (Expr_ a)


