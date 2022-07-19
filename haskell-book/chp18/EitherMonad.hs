module EitherMonad where

data MySum a b = First a | Second b 
  deriving (Eq, Show)

instance Functor (MySum a) where
  fmap _ (First x) = First x
  fmap f (Second y) = Second (f y)


instance Applicative (MySum a) where
  pure = Second 

  First x <*> _ = First x
  Second _ <*> First y = First y
  Second g <*> Second x = Second (g x)

instance Monad (MySum a) where
  return = pure

  (>>=) :: MySum a b -> (b -> MySum a c) -> MySum a c
  First x >>= _ = First x  
  Second x >>= f = f x


{- EG:
λ> (Left 1) >>= (\x->Right (x+1))
Left 1
λ> (First 1) >>= (\x->Second (x+1))
First 1

λ> (Right 1) >>= (\x->Right (x+1))
Right 2
λ> (Second 1) >>= (\x->Second (x+1))
Second 2

-}


