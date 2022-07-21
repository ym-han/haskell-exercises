{-# LANGUAGE InstanceSigs #-}


-- 16.10
module FunctorExs where

-- fmap id = id
-- (fmap g (fmap f x)) == (fmap (g . f) x)

-- q1
newtype MyIdentity a = MyIdentity a

instance Functor MyIdentity where
  fmap f (MyIdentity x) = MyIdentity $ f x

-- q5
data Three' a b = Three' a b b
instance Functor (Three' a) where
  fmap f (Three' x y z) = (Three' x (f y) (f z))


-- q7
data Four' a b = Four' a a a b
instance Functor (Four' a) where
  fmap f (Four' q r s t) = (Four' q r s (f t) )

-- 16.17
--- p668 q2
data Company a b c = 
    DeepBlue a c
  | Something b

instance Functor (Company e e') where 
  fmap _ (Something b) = Something b
  fmap f (DeepBlue a c) = DeepBlue a (f c)


--- p669 q2

data K a b = K a

instance Functor (K a) where
  fmap _ (K a) = K a

--- p670 q9

data List a = Nil
            | Cons a (List a)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons first rest) = Cons (f first) (fmap f rest)

---

data TalkToMe a = Halt
                | Print String a
                | Read (String -> a)



