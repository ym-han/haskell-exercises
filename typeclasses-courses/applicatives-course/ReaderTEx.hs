{-# LANGUAGE InstanceSigs #-}

module ReaderTEx where

import Control.Applicative


newtype ReaderT env f a = ReaderT (env -> f a)

type ReaderIO env a = ReaderT env IO a

-- write the Functor and Applicative instances for ReaderT

instance Functor f => Functor (ReaderT env f) where
  fmap :: (a -> b) -> ReaderT env f a
                   -> ReaderT env f b
  fmap g (ReaderT h) = ReaderT (\env -> (fmap g (h env)))
  -- h :: env -> f a 

instance Applicative f => Applicative (ReaderT env f) where
  pure :: a -> ReaderT env f a
  pure x = ReaderT (\env -> pure x)

  {-
    g :: env -> f (a -> b)
    x :: env -> f a

    If we let m := `(->) env', 
    g :: m (f (a->b)) 
    x :: m (f a) 
  -}
  (<*>) :: ReaderT env f (a -> b) -> ReaderT env f a -> ReaderT env f b
  ReaderT g <*> ReaderT x = ReaderT ( \env -> (g env) <*> (x env) )


  -- via peeking at the typeclasses.com solution...
  liftA2 :: (a -> b -> c) -> ReaderT env f a -> ReaderT env f b -> ReaderT env f c
  liftA2 g (ReaderT x) (ReaderT y) = ReaderT ( (liftA2 . liftA2) g x y )
  {-
    liftA2 . liftA2 :: (a -> b -> c) -> m (f a) -> m (f b) -> m (f c)
                  x :: m (f a) 
                  y :: m (f b)
  -}

