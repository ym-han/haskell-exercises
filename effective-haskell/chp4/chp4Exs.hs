{-# LANGUAGE LambdaCase, OverloadedStrings #-}

module Chp4Exs where

------- 1. Bin Trees
data Tree a = Leaf | Branch (Tree a) a (Tree a) deriving (Eq,Ord,Show)
        

eg = Branch Leaf "hi" Leaf
eg2 = Branch eg "2hi" Leaf
eg3 = Branch Leaf "2hi" eg
eg4 = Branch eg3 "bleh" eg2


egi1 :: Tree Int
egi1 = Branch Leaf (3::Int) Leaf
egi2 = Branch egi1 (7::Int) Leaf
--- The key invariant for BSTs: everything in the left sub-tree is less than the value in the root and similarly, everything in the right sub-tree is greater than the root

-- Add a new integer into a binary search tree of integers
insertInt :: Tree Int -> Int -> Tree Int 
insertInt Leaf = Branch Leaf n Leaf
insertInt @bt(Branch left val right) = case compare n val of
        EQ -> bt
        LT -> Branch (insertInt left n) val right
        GT -> Branch left val (insertInt right n) 

        
-- Check to see if an int value exists in a binary search tree of ints
isIn :: Tree Int -> Int -> Bool
isIn Leaf _ = False
isIn (Branch left val right) n
  | n == val = True
  | n < val = isIn left n
  | n > val = isIn right n

