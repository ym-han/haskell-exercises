module Chp15Optional where

-- page 390+
import Data.Monoid

data Optional a = 
    Nada
  | Only a
  deriving (Eq, Show)


instance Semigroup a => Semigroup (Optional a) where
  -- mappend :: o a -> o a -> o a
  (<>) Nada other = other
  (<>) other Nada = other
  (<>) (Only x) (Only y) = Only (x <> y)


instance Monoid a 
     => Monoid (Optional a) where
  mempty = Nada


{-
Expected output:

onlySum = Only (Sum 1)
onlySum `mappend` onlySum == Only (Sum {getSum = 2})

basically combining the wrapped values

OK seems to work
-} 

