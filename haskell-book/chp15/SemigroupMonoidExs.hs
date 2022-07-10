-- Section 15.15 p.609+: Semigroup and Monoid exercises
--- TO DO: Add hedgehog tests


import Data.Monoid

--- 1. 
data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
  -- (<>) :: a -> a -> a
  Trivial <> Trivial = Trivial

instance Monoid Trivial where
  mempty = Trivial

{-
λ> Trivial <> mempty
Trivial
λ> mempty <> Trivial
Trivial

(Trivial <> (Trivial <> Trivial)) == ((Trivial <> Trivial) <> Trivial)
-}

--- Semigroup and Monoids 2.
newtype Identity a = Identity a deriving Show

instance Semigroup a => Semigroup (Identity a) where
  Identity x <> Identity y = Identity (x <> y)

instance Monoid a => Monoid (Identity a) where
  mempty = Identity mempty

{-
Re <>: We can't, e.g., just go for the left or right elt, 
since that would violate associativity.
This is the only other natural choice.

λ> Identity "1" <> mempty
Identity "1"
λ> mempty <> Identity "1"
Identity "1"
λ> Identity "a" <> Identity "h"
Identity "ah"

λ> (Identity "a" <> Identity "b") <> Identity "c"
Identity "abc"
λ> Identity "a" <> (Identity "b" <> Identity "c")
Identity "abc"
-}

-- 3. 
data Two a b = Two a b deriving (Eq, Show)
instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  (Two q r) <> (Two s t) = Two (q <> s) (r <> t)

instance (Monoid a, Monoid b) => Monoid (Two a b) where
  mempty = Two mempty mempty


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

λ> Two "a" "b" <> mempty
Two "a" "b"
λ> mempty <> Two "a" "b"
Two "a" "b"

-}


-- Semigroup ex. 4. 
data Three a b c = Three a b c deriving (Eq, Show)
instance (Semigroup a, Semigroup b, Semigroup c) => Semigroup (Three a b c) where
  (Three a b c) <> (Three d e f) = Three (a <> d) (b <> e) (c <> f)


-- Semigroup ex. 5 / Monoid ex 4. 
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


-- Semigroup 11 (Validation)

data Validation a b = Failure a | Success b deriving (Eq, Show)

instance Semigroup a =>
  Semigroup (Validation a b) where
    Success s <> _ =  Success s
    _ <> Success s =  Success s
    Failure f <> Failure f' = Failure (f <> f')

-- for testing, from book
testVal :: IO () 
testVal = do
  let failure :: String
              -> Validation String Int 
      failure = Failure
      success :: Int
              -> Validation String Int
      success = Success
  print $ success 1 <> failure "blah" 
  print $ failure "woot" <> failure "blah" 
  print $ success 1 <> success 2
  print $ failure "woot" <> success 2

{- Expected output:
λ> testVal
Success 1
Failure "wootblah"
Success 1
Success 2
-}


-- Monoid 6. 
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


-- Monoid 7. 
newtype Comp a = Comp (a -> a)

instance Semigroup (Comp a) where
  Comp f <> Comp g = Comp $ f . g

instance Monoid (Comp a) where
  mempty = Comp id


-- Monoid 8. threading state through computations

newtype Mem s a = Mem { runMem :: s -> (a,s) }

instance Semigroup a => Semigroup (Mem s a) where 
  x <> y = Mem (\s -> 
    let (a, s') = (runMem x) s
        (a', s'') = (runMem y) s'
    in (a <> a', s''))

instance Monoid a => Monoid (Mem s a) where 
  mempty = Mem (\s -> (mempty, s))

f' = Mem $ \s -> ("hi", s + 1)

testMem :: IO ()
testMem = do
  let rmzero = runMem mempty 0
      rmleft = runMem (f' <> mempty) 0
      rmright = runMem (mempty <> f') 0 
  print $ rmleft
  print $ rmright
  print $ (rmzero :: (String, Int)) 
  print $ rmleft == runMem f' 0 
  print $ rmright == runMem f' 0

{-
λ> testMem
("hi",1)
("hi",1)
("",0)
True
True
-}




