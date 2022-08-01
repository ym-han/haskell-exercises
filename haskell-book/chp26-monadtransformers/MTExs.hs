{-# LANGUAGE InstanceSigs #-}

module MTExs where

import Control.Applicative
import Control.Monad (ap)

newtype EitherT e m a = EitherT { runEitherT :: m (Either e a) }

instance Functor m 
      => Functor (EitherT e m) where
  fmap :: Functor m => (a -> b) -> EitherT e m a -> EitherT e m b
  fmap f (EitherT ema) = EitherT $ (fmap . fmap) f ema

instance Applicative m
      => Applicative (EitherT e m) where
  pure :: Applicative m => a -> EitherT e m a
  pure x = EitherT (pure (pure x)) 

  (<*>) :: Applicative m 
      => EitherT e m (a -> b) -> EitherT e m a -> EitherT e m b
  (EitherT f) <*> (EitherT a)  = EitherT $ (liftA2 (<*>)) f a
  -- the trick of specifying annotated but undefined components, 
--   and then checking the types using the compiler, was helpful for getting this

instance Monad m 
      => Monad (EitherT e m) where
   return = pure
   (>>=) :: Monad m => EitherT e m a -> (a -> EitherT e m b) -> EitherT e m b
   (EitherT mea) >>= f = 
       EitherT $ do 
          ea <- mea
          case ea of 
            Left e -> return (Left e)
            Right a -> runEitherT (f a)   

