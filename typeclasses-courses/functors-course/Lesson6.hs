module Lesson6 where


import Data.Bifunctor
import Data.Align

-- Exercise: write Functor Bifunctor instances for this type
data These a b = This a | That b | These a b 
    deriving Show

instance Functor (These a) where
  fmap f (This l) = This l
  fmap f (That b) = That (f b)
  fmap f (These l r) = These l (f r)


instance Bifunctor These where
  bimap f _ (This a) = This (f a)
  bimap _ g (That b) = That (g b)
  -- This and That like Left and Right of Either
  bimap f g (These a b) = These (f a) (g b)    



{-
λ> fmap (+4) (This 5)
This 5
λ> fmap (+4) (That 5)
That 9
λ> bimap (+4) not (These 5 True)
These 9 False
λ> first (+4) (This 5)
This 9
λ> first (+4) (That 5)
That 5
-}
