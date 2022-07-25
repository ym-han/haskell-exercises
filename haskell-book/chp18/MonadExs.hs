{-# LANGUAGE InstanceSigs #-}

module MonadExs where

import Control.Monad (ap)


data Nope a = NopeDotJpg

instance Functor Nope where
  fmap f NopeDotJpg = NopeDotJpg

instance Applicative Nope where 
  pure _ = NopeDotJpg   
  NopeDotJpg <*> NopeDotJpg = NopeDotJpg


instance Monad Nope where
  return = pure

  (>>=) :: Nope a -> (a -> Nope b) -> Nope b
  NopeDotJpg >>= _ =  NopeDotJpg

----------
newtype Id a = Id a deriving (Eq, Ord, Show)

instance Functor Id where 
  fmap f (Id x) = Id (f x) 

instance Applicative Id where
  pure x = Id x
  Id f <*> Id x = Id $ f x
  
instance Monad Id where
  return = pure
  Id x >>= f = f x    

----------
data List a = Nil
            | Cons a (List a)

instance Semigroup (List a) where
  Nil <> xs = xs
  xs <> Nil = xs
  Cons x xs <> ys = Cons x (xs <> ys)

instance Monoid a => Monoid (List a) where 
  mempty = Nil

instance Functor List where
  fmap f xs = xs >>= return . f

instance Applicative List where
  pure x = Cons x Nil
  (<*>) :: List (a -> b) -> List a -> List b
  (<*>) = ap

instance Monad List where
  return = pure

  (>>=) :: List a -> (a -> List b) -> List b
  Nil >>= _ = Nil  
  Cons x xs >>= f = (f x) <> (xs >>= f)

----------

j :: Monad m => m (m a) -> m a
j m = m >>= id

l1 :: Monad m => (a -> b) -> m a -> m b
l1 f ma = ma >>= (\x -> return $ f x)

l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 f ma mb = ma >>= (\a -> 
             mb >>= (\b ->
             return $ f a b))

a :: Monad m => m a -> m (a -> b) -> m b
a ma mf = do a <- ma
             f <- mf
             return $ f a 
{-
EG:
λ> a [1, 2, 3] [(\x->x+1::Float), (\x->x*100::Float)] 
[2.0,100.0,3.0,200.0,4.0,300.0]
-}