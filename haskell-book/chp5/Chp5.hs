module Chp5 where

-- Q2: Write both possible implementations of a function with type signature a -> a -> a
q2_fst :: a -> a -> a
q2_fst = curry (\(x, y) -> x)

q2_snd :: a -> a -> a
q2_snd = curry (\(x,y) -> y)

-- Q3: Implement a -> b -> b

q3_fst :: a -> b -> b
q3_fst _ = id

-- I think we're basically forced to return the second arg