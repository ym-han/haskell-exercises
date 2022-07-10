-- Section 15.15 p.609+
--- TO DO: Add hedgehog tests


import Data.Monoid

--- 1. 
data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
  -- (<>) :: a -> a -> a
  Trivial <> Trivial = Trivial


--- 2.
newtype Identity a = Identity a

instance Semigroup a => Semigroup (Identity a) where
  Identity x <> Identity y = Identity (x <> y)

{-
Re <>: We can't, e.g., just go for the left or right elt, 
since that would violate associativity.
This is the only other natural choice.

-}

-- 3. 
data Two a b = Two a b deriving (Eq, Show)
instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  (Two q r) <> (Two s t) = Two (q <> s) (r <> t)

{- EG of output
λ> Two (Sum 1) (Sum 3) <> Two (Sum 5) (Sum 9)
Two (Sum {getSum = 6}) (Sum {getSum = 12})


λ> Two (Product 1) (Product 3) <> Two (Product 5) (Product 9)
Two (Product {getProduct = 5}) (Product {getProduct = 27})

λ> Two (Sum 1) (Product 3) <> Two (Sum 5) (Product 9)
Two (Sum {getSum = 6}) (Product {getProduct = 27})

We get the same thing with tuples:
λ> (Sum 1, Product 3) <> (Sum 5, Product 9)
(Sum {getSum = 6},Product {getProduct = 27})
-}


-- 4. 
data Three a b c = Three a b c deriving (Eq, Show)
instance (Semigroup a, Semigroup b, Semigroup c) => Semigroup (Three a b c) where
  (Three a b c) <> (Three d e f) = Three (a <> d) (b <> e) (c <> f)


-- 5. 
newtype BoolConj = BoolConj Bool deriving Show
instance Semigroup BoolConj where
  (BoolConj True) <> (BoolConj True) = (BoolConj True)
  (BoolConj _) <> (BoolConj _) = (BoolConj False)

instance Monoid BoolConj where
  mempty = BoolConj True

{-
λ> (BoolConj True) `mappend` mempty
BoolConj True
λ> mempty `mappend` (BoolConj False)
BoolConj False

λ> (BoolConj True) `mappend` (BoolConj False)
BoolConj False
λ> (BoolConj True) `mappend` (BoolConj True)
BoolConj True

-}

-- 6. 
newtype Combine a b = Combine { unCombine :: (a -> b) }
instance (Semigroup b) => Semigroup (Combine a b) where
   f <> g = Combine (\x -> (unCombine f $ x) <> (unCombine g $ x))

instance Monoid b => Monoid (Combine a b) where
  mempty = Combine (\x -> mempty)


{-
Spec:
λ> f = Combine $ \n -> Sum (n + 1)
λ> unCombine (mappend f mempty) $ 1
Sum {getSum = 2}

had to google for the answer. obvious in retrospect -- addition of functions
was fun trying to come up with the mempty though;
that wasn't entirely obvious even with the right implementation of the semigroup
-}


-- 7.
newtype Comp a = Comp (a -> a)

instance Semigroup (Comp a) where
  Comp f <> Comp g = Comp $ f . g

instance Monoid (Comp a) where
  mempty = Comp id


-- 8. threading state through computations

newtype Mem s a = Mem { runMem :: s -> (a,s) }

instance Semigroup a => Semigroup (Mem s a) where 
  x <> y = Mem (\s -> 
    let (a, s') = (runMem x) s
        (a', s'') = (runMem y) s'
    in (a <> a', s''))

instance Monoid a => Monoid (Mem s a) where 
  mempty = Mem (\s -> (mempty, s))



f' = Mem $ \s -> ("hi", s + 1)

main :: IO ()
main = do
  let rmzero = runMem mempty 0
      rmleft = runMem (f' <> mempty) 0
      rmright = runMem (mempty <> f') 0 
  print $ rmleft
  print $ rmright
  print $ (rmzero :: (String, Int)) 
  print $ rmleft == runMem f' 0 
  print $ rmright == runMem f' 0





