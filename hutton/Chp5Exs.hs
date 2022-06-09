module Chp5Exs where

-- Q2: 
grid :: Int -> Int -> [(Int, Int)]
grid n m = [(i, j) | i <- [0..n], j <- [0..m]]

-- Q3: return coord space of size n, but excluding diagonal
squareNoDiag :: Int -> [(Int, Int)]
squareNoDiag n = [(i, j) | (i, j) <- (grid n n), i /= j] 
