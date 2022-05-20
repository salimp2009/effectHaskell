{-# LANGUAGE RankNTypes #-}
module CustomOperators where

-- custom operator 
-- must always have exactly two arguments
-- infix by default
-- s can be named using nearly any combination of ascii and
-- unicode symbols, but they can’t contain letters or spaces
-- _ is considered a letter, no parenthesis () 
-- when the binding precedence for two left associative
-- operators is same, then they are evaluated left-to-right.

(+++) :: Num a => a -> a -> a
(+++) a b  = a + b
infixl 6 +++

increment :: Integer -> Integer
increment = (+++ 1)

divide :: forall a. Fractional a =>  a -> a -> a
divide = (/)