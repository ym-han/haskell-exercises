module L3Ex2 where

liftA3 :: Applicative f => (a -> b -> c -> d) -> f a -> f b -> f c -> f d
liftA3 g x y z = pure g <*> x <*> y <*> z
-- liftA3 g x y z = fmap g <*> x <*> y <*> z
