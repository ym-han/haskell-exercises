module MonadsChp12Exs where

-- -- Q1: define instance of Functor class for
-- data Tree a = Leaf | Node (Tree a) a (Tree a)
--               deriving Show

-- instance Functor Tree where
--   -- fmap :: (a -> b) -> m a -> m b
--   fmap _ Leaf = Leaf
--   fmap g (Node left val right) = Node (fmap g left) (g val) (fmap g right)

-- Q2: Came close, but didn't really manage to get this. Hadn't really understood that (((->) a) b) corresponds to (a -> b)
-- instance Functor ((->) a) where
--   -- fmap :: (b -> c) -> (a -> b) -> (a -> c)
--   fmap = (.)

-- Q3: Define instance of applicative classs for type (a->)
-- again had to look at soln. i think i gave up a bit too soon for this one --- should have thought more carefully about the type signatures
-- instance Applicative (a->) where
--   -- pure :: b -> (a -> b)
--   pure = const
--   -- where const x _ = x

--   -- (<*>) :: (a -> (b -> c)) -> (a -> b) -> (a -> c)
--   mg <*> mb = \x -> (mg x) (mb x)

-- Q4:

newtype MyZipList a = Z [a] deriving Show

instance Functor MyZipList where
    -- fmap :: (a -> b) -> MyZipList a -> MyZipList b
    fmap g (Z xs) = Z (fmap g xs)

instance Applicative MyZipList where
  -- pure :: a -> ZipList a
  pure x = Z (repeat x)

  -- <*> :: ZipList (a -> b) -> ZipList a -> ZipList b
  (Z gs) <*> (Z xs) = Z [fst tup (snd tup) | tup <- zip gs xs]

