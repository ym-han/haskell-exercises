{-# LANGUAGE InstanceSigs #-}

module MTExs where

import Control.Arrow (first, second)
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

--- StateT

newtype StateT s m a = 
  StateT { runStateT :: s -> m (a, s) }

instance (Functor m) => Functor (StateT s m) where
    fmap :: Functor m => (a -> b) -> StateT s m a -> StateT s m b
    fmap f (StateT smas) = StateT $ \s -> 
                                      let mas = smas s
                                      in fmap (first f) mas 


instance (Monad m)
    => Applicative (StateT s m) where
  pure :: Monad m => a -> StateT s m a
  pure a = StateT (\s -> pure (a, s)) 
  (<*>) :: Monad m => StateT s m (a -> b) -> StateT s m a -> StateT s m b
  (StateT smaf) <*> (StateT sma) = StateT (\s -> do 
                                                    (f, s') <- smaf s 
                                                    (a, s'') <- sma s'
                                                    return (f a, s'')) 


instance (Monad m)
    => Monad (StateT s m) where
  return :: Monad m => a -> StateT s m a
  return = pure

  (>>=) :: Monad m => StateT s m a -> (a -> StateT s m b) -> StateT s m b
  (StateT sma) >>= f = StateT $ \s -> do 
                                         (a, s') <- sma s
                                         let ssmb = f a
                                         (b, s'') <- (runStateT ssmb) s'
                                         return (b, s'')
