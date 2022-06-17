# Qns for self-testing, mined from Hutton's book

(A lot of the excerpts below are from Hutton's book.)

1a. What is a functor
1b. What is the type of fmap
1c. A version of fmap for functions with any desired number of arguments can be constructed in terms of what two basic functions, with what types?


2a. What is the type of (<*>)
2b. <*> is a generalized form of what?
      
      <*> is basically a generalized form of function application for which the argument function, the argument value, and the result value are all contained in f structures.
2c. Put `fmap g x' in terms of `pure' and `<*>'

3a. What is an applicative --- give the rough intuition, as it relates to fmap
3b. What is an applicative --- give the class declaration
3c. Suppose we had Maybe as a functor. How would we also make it into an *applicative* functor?
3d. Explain how "the applicative style for Maybe supports a form of *exceptional* programming in which we can apply prue functions to arguments that may fail without the need to manage the propagation of failure ourselves" (p. 160)
3e. Why do we need monads --- why aren't applicatives enough? In particular, what was the use case that was used to motivate monads in Hutton's book?

  When the function that's being used to process the potentially-failing values is itself not a pure function --- when the function can also fail (i.e., is of type a -> M b).


4. What is the definition of >>=?
5. How can we use >>= make the tedious definition of eval on page 104 more concise?  
6. How can we rewrite the definition of eval in terms of >>= using the do sugar?
7. What is a monad? Give the built-in declaration
8. How can lists be made into a monadic type, in a way that lets us easily define a cartesian product function with the do notation?
9. Given that declaration of lists as a monadic type, define a function taht returns all possible ways of pairing elements from two lists; the function should use the do notation

# Applicatives

## How think of them

One way to think about them: Applicative functors generalize the idea of mapping to functions with more than one parameter

Another way: App. functors are an abstraction for applying pure functions to effectful arguments

## Class declaration for applicatives

\begin{code}
class Functor f => Applicative f where
  pure :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b
\end{code}

## How to declare an instance thereof

Declaring Maybe to be an applicative functor, assuming its already been declared to be a functor

\begin{code}
instance Applicative Maybe where
  -- pure :: a -> Maybe a
  pure x = Just x
  -- (<*>) :: Maybe (a -> b) Maybe a -> Maybe b
  Nothing <*> mx = Nothing
  (Just g) <*> mx = fmap g mx
\end{code}

Note that:
* `g` has type a -> b
* `mx` has type Maybe a
* `fmap g mx` has type Maybe b

### Examples of usage

How exactly does 

> pure (+) <*> Just 1 <*> Just 2 

work? Why does it give us Just 3?

(pure (+) <*> Just 1) <*> Just 2 
= (Just (+) <*> Just 1) <*> Just 2 
= (fmap (+) Just 1) <*> Just 2
= Just (+1) <*> Just 2  
= fmap (+1) Just 2
= Just (2+1)
= Just 3

In short, it's because we are using a binary function, and because we are feeding the args to the function one by one / fmapping them one by one!


Define a function that returns all possible ways of multiplying two lists of integers using applicatives

> prods :: [Int] -> [Int] -> [Int]
> prods xs ys = pure (*) <*> xs <*> ys

-- λ> prods [1, 2, 3] [4, 5, 6]
-- [4,5,6,8,10,12,12,15,18]

### Qns about applicatives

Hutton says that there is at most only one way to make any given parameterized type into a functor. Does the analogous thing hold also for applicatives? That is, suppose we have a functor --- is there also only one way to declare it to be an applicative functor? 

Answer: No. Counterexamples include ZipList in Control.Applicative and Either e ('Validation'). (Thanks to Arc in the FP discord for giving me these counterexamples.)



# Monads

## Monads via Eval

eval :: Expr -> Maybe Int

### Using the bind operator >>= to make eval more concise

\begin{code}
(>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
mx >>= f = case mx of 
            Nothing -> Nothing
            Just x -> f x 
\end{code}

"That is, >>= takes an argument of type a that may fail and a function of type a -> b whose result may fail, and returns a result of type b that may fail. If the argument fails we propagate the failure, otherwise we apply the function to the resulting value. In this manner >>= integrates the sequencing of values of type Maybe with the processing of their results. The >>= operator is often called bind, because the second argument binds the result of the first"


eval (Val n) = Just n
eval (Div x y) = eval x >>= \n ->
                 eval x >>= \m ->
                 safediv n m

### Sugaring away the >>=: The final version of eval

eval :: Expr -> Maybe Int
eval (Val n) = Just n
eval (Div x y) = do n <- eval x
                    m <- eval x
                    safediv n m


A **monad** is an applicative type m that supports `return' and >>= functions of the specified types.

The built-in declaration for a monad:

\begin{code}
class Applicative m => Monad m where
  return :: a -> m a
  (>>=)  :: m a -> (a -> m b) -> m b

  return = pure
\end{code}

## Examples

How to make lists into monadic type

\begin{code}
instance Monad [] where 
  x >>= f = concat (map f xs)
\end{code}



## State monad and State transformers

newtype ST a = S (State -> (a, State))

app :: ST a -> State -> (a, State)
app (S st) x = st x

instance Monad ST where
  -- return :: a -> ST a
  return x = S (\s -> let (x, s') = app st s in app (f x) s')






