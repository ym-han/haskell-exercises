{-# LANGUAGE InstanceSigs #-}

module Chp25 where

newtype Compose f g a = 
    Compose { getCompose :: f (g a) } 
    deriving (Eq, Show)


instance (Functor f, Functor g) => 
        Functor (Compose f g) where
    fmap f (Compose fga) =
        Compose $ (fmap . fmap) f fga


instance (Applicative f, Applicative g) => Applicative (Compose f g) where
    pure :: a -> Compose f g a 
    pure = Compose . pure . pure

    (<*>) :: Compose f g (a -> b) 
            -> Compose f g a
            -> Compose f g b
    (Compose fgab) <*> (Compose fga) = 
        Compose $ (<*>) <$> fgab <*> fga

{-
λ> :t (fmap (<*>))
(fmap (<*>))
  :: forall {f :: * -> *} {g :: * -> *} {a} {b}.
     (Applicative f, Functor g) =>
     g (f (a -> b)) -> g (f a -> f b)

g f a 

-> g f b

λ> :t (<*>)
(<*>)
  :: forall (f :: * -> *) a b.
     Applicative f =>
     f (a -> b) -> f a -> f b


_fagb :: f (a -> g b)

_2 :: f a
-}