{-# LANGUAGE InstanceSigs #-}

module L4Exs where

import Control.Applicative 

{-
Ex 1
Write a function like zipApp that mimics the ZipList liftA2 
instead of mimicking <*> for ZipList. 
In other words, you’re writing zipWith. 
As with our implementation of zipApp above, you do not need to use 
the ZipList newtype for this one.

λ> :type zipWith
zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
-}


myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith _ [] _ = []
myZipWith _ _ [] = []
myZipWith f (x:xs) (y:ys) = f x y : myZipWith f xs ys

{-
Ex 2
Write an Applicative instance for ZipList.

-}

zipApp :: [(a -> b)] -> [a] -> [b]
zipApp [] _ = []
zipApp _ [] = []
zipApp (f:fs) (x:xs) = f x : zipApp fs xs

newtype MyZipList a = MyZipList { getMyZipList :: [a] }
  deriving Show


instance Functor MyZipList where
  fmap f lst = MyZipList {getMyZipList = fmap f (getMyZipList lst)}

instance Applicative MyZipList where
  pure x = MyZipList {getMyZipList = repeat x}

  (<*>) :: MyZipList (a -> b) -> MyZipList a -> MyZipList b
  gs <*> ys = 
    let glst = getMyZipList gs 
        ylst = getMyZipList ys
    in MyZipList (zipApp glst ylst)











