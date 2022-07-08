-- working out the types of composed fmaps

(.) :: (b -> c) -> (a -> b) -> a -> c

fmap1 :: Functor f => (m -> n) -> f m -> f n
fmap2 :: Functor g => (x -> y) -> g x -> g y


Substituting, we get

a := (x -> y) 
b := (m -> n) = (g x -> g y)
c := (f m -> f n) 


    (g x -> g y) -> (fg x -> fg y)   --- b -> c 
->  (x -> y) -> (g x -> g y)         --- a -> b

->  (x -> y)                         --- a
->  (fg x -> fg y)                   --- c


\begin{code}
-- const :: q -> r -> q
replaceWithP = const 'p'
-- so replaceWithP :: r -> Char

-- lms - List (Maybe String)
lms = [Just "Ave", Nothing, Just "woohoo"]
\end{code}


And the `a' will just be replaceWithP in this context,
so that fmap . fmap replaceWithP :: fg r -> fg Char,
where f and g are functor1 and functor2 respectively.

Indeed, if we ask ghci for the types:
```
λ> :t (fmap . fmap) replaceWithP
(fmap . fmap) replaceWithP
  :: forall {f1 :: * -> *} {f2 :: * -> *} {a}.
     (Functor f1, Functor f2) =>
     f1 (f2 a) -> f1 (f2 Char)
'''

And similarly with multiple composed fmaps:

```
λ> :t (fmap . fmap . fmap) replaceWithP
(fmap . fmap . fmap) replaceWithP
  :: forall {f1 :: * -> *} {f2 :: * -> *} {f3 :: * -> *} {a}.
     (Functor f1, Functor f2, Functor f3) =>
     f1 (f2 (f3 a)) -> f1 (f2 (f3 Char))
'''

\begin{code}

\end{code}