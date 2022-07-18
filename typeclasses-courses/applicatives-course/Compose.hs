{-# LANGUAGE InstanceSigs #-}

module Compose where

import Control.Applicative

newtype Compose f g a =
  Compose { getCompose :: f (g a) }


instance (Functor f, Functor g) => Functor (Compose f g) where
  fmap f (Compose x) = Compose (fmap (fmap f) x)

instance (Applicative f, Applicative g) => Applicative (Compose f g) where
  pure :: a -> (Compose f g a)
  pure x = Compose (pure (pure x))

  liftA2 :: (a->b->c) -> Compose f g a -> Compose f g b -> Compose f g c
  liftA2 h (Compose x) (Compose y) = Compose $ (liftA2 . liftA2) h x y