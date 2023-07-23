{-# LANGUAGE InstanceSigs #-}
module ReaderExs where

import Control.Applicative
import Data.Maybe
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Data.Char

--- ex p850
cap :: [Char] -> [Char] 
cap = map toUpper
rev :: [Char] -> [Char] 
rev = reverse


composed :: [Char] -> [Char] 
composed = cap . rev

fmapped :: [Char] -> [Char]
fmapped = cap <$> rev

tupledApp :: [Char] -> ([Char], [Char])
tupledApp = (,) <$> rev <*> cap
{-
λ> :t ((,) <$> rev)
((,) <$> rev) :: [Char] -> b -> ([Char], b)

Breaking down fmap (,) rev:
    [Char] -> (w -> ([Char], w))    ---- (a -> b), where a := [Char], b := (w -> ([Char], w))
->  ([Char] ->) [Char]              ---- f a
->  ([Char] ->) (w -> ([Char], w))  ---- f b

-}

-- this results in different output
tupledComposed :: [Char] -> ([Char], [Char])
tupledComposed = (,) <*> (rev. cap)

----- ex p860 implement reader applicative
newtype Reader r a = Reader { runReader :: r -> a }

instance Functor (Reader r) where 
    fmap f (Reader ra) = Reader $ \r -> f (ra r)

instance Applicative (Reader r) where 
  pure :: a -> Reader r a
  pure a = Reader $ \r -> a

  (<*>) :: Reader r (a -> b) 
        -> Reader r a
        -> Reader r b
  (Reader rab) <*> (Reader ra) =
    Reader $ \r -> (rab r) (ra r)


---- ex p864 implement reader monad
instance Monad (Reader r) where
  return = pure

  (>>=) :: Reader r a
        -> (a -> Reader r b)
        -> Reader r b
  (Reader ra) >>= aRb = 
    Reader $ \r -> (runReader $ aRb (ra r)) r 
    -- this feels too complicated

---- ex p867 end of chap exercises

x = [1, 2, 3] 
y = [4, 5, 6] 
z = [7, 8, 9]

-- lookup :: Eq a => a -> [(a, b)] -> Maybe b

-- zip x and y using 3 as the lookup key
xs :: Maybe Integer 
xs = lookup 3 $ zip x y

-- zip y and z using 6 as the lookup key
ys :: Maybe Integer 
ys = lookup 6 $ zip y z

-- zip x and y using 4 as the lookup key
zs :: Maybe Integer
zs = lookup 4 $ zip x y


-- now zip x and z using a
-- variable lookup key
z' :: Integer -> Maybe Integer 
z' n = lookup n $ zip x z

x1 :: Maybe (Integer, Integer) 
x1 = liftA2 (,) xs ys

x2 :: Maybe (Integer, Integer) 
x2 = liftA2 (,) ys zs

x3 :: Integer
   -> (Maybe Integer, Maybe Integer)
x3 = \i -> (z' i, z' i)


-- uncurry :: (a -> b -> c) -> (a, b) -> c
summed :: Num c => (c, c) -> c
summed = uncurry (+)

bolt :: Integer -> Bool
-- use &&, >3, <8
bolt = liftA2 (&&) (>3) (<8)

sequA :: Integral a => a -> [Bool]
sequA m = sequenceA [(>3), (<8), even] m

foldrSequaA :: Integral a => a -> Bool
foldrSequaA m = foldr (&&) True (sequA m)

s' :: Maybe Integer
s' = summed <$> ((,) <$> xs <*> ys)



main :: IO ()
main = do
  print $
    sequenceA [Just 3, Just 2, Just 1]
    
  print $ sequenceA [x, y] 
  print $ sequenceA [xs, ys]
  print $ summed <$> ((,) <$> xs <*> ys) 
  print $ fmap summed ((,) <$> xs <*> zs) 
  print $ bolt 7
  print $ fmap bolt z

  print $ sequenceA [(<3), (<8), even] 7

  print $ foldrSequaA
  print $ sequA $ fromMaybe 0 s'
  print $ bolt $ fromMaybe 0 ys