module TravExs where

import Hedgehog
import           Hedgehog.Classes
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

newtype MyIdentity a = MyIdentity a
  deriving (Eq, Ord, Show)


instance Functor MyIdentity where
  fmap f (MyIdentity x) = MyIdentity $ f x

instance Foldable MyIdentity where
  foldMap f (MyIdentity x) = f x


instance Traversable MyIdentity where
  traverse f (MyIdentity x) = fmap MyIdentity (f x)

genIdentity :: Gen a -> Gen (MyIdentity a)
genIdentity ga = MyIdentity <$> ga

--------

newtype Constant a b = Constant { getConstant :: a } deriving (Eq, Show)

instance Functor (Constant a) where
  fmap _ (Constant x) = Constant x

instance Foldable (Constant a) where
  foldMap _ (Constant _) = mempty

instance Traversable (Constant a) where
  traverse f (Constant a) = pure (Constant a)

genConst :: Gen a -> Gen (Constant a b)
genConst ga = Constant <$> ga
-- TO DO: Figure out how to use hedgehog classes to check the laws

------------
data Optional a = Nada | Yep a deriving (Eq, Show)

instance Functor Optional where
  fmap f Nada = Nada
  fmap f (Yep x) = Yep (f x)

instance Foldable Optional where
  foldMap _ Nada = mempty
  foldMap f (Yep x) = f x

instance Traversable Optional where
  traverse _ Nada = pure Nada
  traverse f (Yep x) = Yep <$> f x

------------

data Three a b c = Three a b c deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three x y z) = Three x y (f z)

instance Foldable (Three a b) where
  foldMap f (Three x y z) = f z

instance Traversable (Three a b) where
  traverse f (Three x y z) = fmap (Three x y) (f z)

------------

data Big a b = Big a b b

instance Functor (Big a) where
  fmap f (Big x y z) = Big x (f y) (f z)

instance Foldable (Big a) where
  foldMap f (Big x y z) = f y <> f z

instance Traversable (Big a) where
  traverse f (Big x y z) = pure (Big x) <*> f y <*> f z  
  -- or: Big x <$> f y <*> f z

------------

data Tree a = Empty
            | Leaf a
            | Node (Tree a) a (Tree a) 
            deriving (Eq, Show)

instance Functor Tree where
  fmap f (Leaf x) = Leaf $ f x
  fmap f (Node left val right) = Node (fmap f left) (f val) (fmap f right)

instance Foldable Tree where
  foldMap f (Leaf x) = f x
  foldMap f (Node left val right) = (foldMap f left) <> f val <> (foldMap f right)

instance Traversable Tree where
  traverse g (Leaf x) =  Leaf <$> g x
  traverse g (Node left val right) = pure Node <*> traverse g left <*> g val <*> traverse g right

{-
Examples:
g = \x -> Just (x + 1) if x > 0 else Nothing
traverse g (Node (Leaf 1) 2 (Leaf 0)) = Nothing
traverse g (Node (Leaf 1) 2 (Leaf 3)) = Just (Node (Leaf 2) 3 (Leaf 4))

-}
------------
main :: IO ()
main = do 
  lawsCheck (traversableLaws genIdentity)
  return ()
