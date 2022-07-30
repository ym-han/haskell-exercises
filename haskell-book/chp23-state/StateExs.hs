{-# LANGUAGE InstanceSigs, LambdaCase #-}
module StateExs where

import Control.Applicative
import Data.Maybe
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Data.Char

newtype State s a = State { runState :: s -> (a, s) }

instance Functor (State s) where
  fmap f (State st) = State (\s -> let (a, s') = st s 
                                    in (f a, s')) 

instance Applicative (State s) where
  pure :: a -> State s a
  pure a = State (\s -> (a, s))

  (<*>) :: State s (a -> b) -> State s a -> State s b
  (State sg) <*> (State sx) = State (\s -> let (g, s') = sg s
                                               (x, s'') = sx s'
                                           in (g x, s'')) 
  -- Note: (<*>) specifies an order in which the state is used: 
  -- left-hand (function) argument first, then the right-hand argument.
  -- http://cmsc-16100.cs.uchicago.edu/2016/Lectures/18-state-monad-1.php


instance Monad (State s) where
  return = pure

  (>>=) :: State s a -> (a -> State s b) -> State s b
  (State sa) >>= g = State (\s -> let (a, s') = sa s
                                      sb = runState (g a)
                                      (b, s'') = sb s'
                                  in (b, s''))

