module CurryHowardIsomorphism where

{-
Algebra     Logic       Types
a + b       a ∨ b       Either a b
a × b       a ∧ b       (a, b)
b^a         a ⇒ b       a -> b
a = b       a ⇐⇒ b     isomorphism
0              ⊥        Void
1              ⊤        ()
-}


-- | proof (a^b)^c = a ^ (b x c)
-- (b -> c -> a) -> (b, c) -> a
muncurry :: (c -> b -> a) -> (b, c) -> a
muncurry f (b, c) = f c b

mcurry :: ((b,c) -> a) -> c -> b -> a
mcurry f c b = f (b,c)

-- | proof for exponent law; a^b x a^c = a^(b+c)
-- to;  (b -> a , c ->a) -> Either b c -> a 
-- from; (Either b c -> a) -> (b -> a , c ->a) 
fromexponent :: (Either b c -> a) -> (b -> a , c ->a)
fromexponent f  = (f.Left, f.Right)

toexponent :: (b -> a) -> (c -> a) -> Either b c -> a
toexponent f _ (Left b)  =  f b 
toexponent _ g (Right c) =  g c

-- | proof for; (a x b)^c = a^c x b^c
-- toproduct;   c -> (a, b) -> (c->a , c -> b)
-- fromproduct; (c->a) -> (c -> b) -> c -> (a, b)

fromproduct :: (c->a) -> (c -> b) -> c -> (a, b)
fromproduct f g c = (f c ,  g c)

toproduct :: (c -> (a, b)) -> (c->a , c -> b)
toproduct f  = (fst . f , snd .f)