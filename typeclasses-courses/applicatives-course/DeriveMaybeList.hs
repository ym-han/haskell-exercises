{-# LANGUAGE DeriveFunctor, DerivingVia #-}

module DeriveMaybeList where

import Data.Functor.Compose


newtype MaybeList a = MaybeList (Maybe [a])
    deriving Show
    deriving Functor
    deriving Applicative via Compose Maybe []
