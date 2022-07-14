module AppL1 where


-- LiftA2 for Maybe
myLiftA2 :: (a -> b -> c)
            -> Maybe a -> Maybe b -> Maybe c
-- myLiftA2 f x y = pure f <*> x <*> y
myLiftA2 f Nothing _ = Nothing
myLiftA2 f (Just _) Nothing = Nothing
myLiftA2 f (Just x) (Just y) = Just (f x y)


-- instance of applicative for Either

data MyEither a b = MyLeft a | MyRight b

instance Functor (MyEither a) where
  fmap g (MyLeft l)  = MyLeft l
  fmap g (MyRight r) = MyRight (g r)

instance Applicative (MyEither a) where
  pure = MyRight
  -- (<*>) :: f (a -> b) -> f a -> f b
  MyLeft l <*> _ = MyLeft l
  MyRight g <*> v = fmap g v  


