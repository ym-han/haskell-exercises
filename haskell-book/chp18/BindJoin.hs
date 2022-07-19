{-# LANGUAGE InstanceSigs #-}

module BindJoin where

import Control.Monad

bindFlipped :: Monad m => (a -> m b ) -> m a -> m b
bindFlipped f x = join (fmap f x)