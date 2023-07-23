{-# LANGUAGE InstanceSigs, TypeApplications #-}

module FoldableExs where


-- import Data.Semigroup
import Data.Monoid
import Data.Foldable

-- Implement the functions in terms of foldMap or foldr from Foldable, 
-- then try them out with multiple types that have Foldable instances.

mysum :: (Foldable t, Num a) => t a -> a
mysum xs = getSum $ foldMap Sum xs 

myelem :: (Foldable t, Eq a) => a -> t a -> Bool
myelem elt xs = getAny $ foldMap (Any . (==elt)) xs

{-
Any :: Bool -> Any

λ> (Any . (==5)) 5
Any {getAny = True}
-}

myToList :: (Foldable t) => t a -> [a]
myToList xs = foldr (:) [] xs

-- realized from stackoverflow that don't need accumulator for min
mymin :: (Foldable t, Ord a) => t a -> Maybe a
mymin xs = let xslst =  myToList xs 
           in mymin' xslst 
           where mymin' [] = Nothing
                 mymin' (x:_) =  Just (foldl' mymin'' x xs)
                 mymin'' x y = if x < y then x else y


mynull :: (Foldable t) 
       => t a -> Bool
mynull = getAny . foldMap (const (Any True))
-- suggestion from discord 
-- the actual, quite clever default impl is null = foldr (\_ _ -> False) True

myfold :: (Foldable t, Monoid m) => t m -> m
myfold xs = foldMap id xs


-- 20.6

data MyConstantB a b = MyConstantB b

instance Foldable (MyConstantB a) where
  foldr f base (MyConstantB y) = f y base
  foldMap f (MyConstantB y) = f y


data Two a b = Two a b

instance Foldable (Two a) where
  foldr f base (Two x y) = f y base
  foldMap f (Two x y) = f y


data Three' a b = Three' a b b deriving (Eq, Show)

instance Foldable (Three' a) where
  foldMap f (Three' x y z) = f y <> f z

{-
λ> foldMap (\x->Sum $ x+7) (Three' 1 11 10)
Sum {getSum = 35}
-}