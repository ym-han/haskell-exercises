module CountdownNotes where

-- unsafe indexing
myidx :: [a] -> Int -> a
myidx (x:_) 0 = x
myidx (x:xs) i = myidx xs (i-1)


-- with just explicit recursion. don't think we need acc here
allButIdx :: [a] -> Int -> [a]
allButIdx (x:xs) 0 = xs
allButIdx (x:xs) idx = x : allButIdx xs (idx-1)


-- Hutton's interleave implementation
-- returns all possible ways of inserting a new elt into a list
interleave :: a -> [a] -> [[a]]
interleave x [] = [[x]]
interleave x (y:ys) = (x:y:ys) : map (y:) (interleave x ys)


{-
to see how this works: 
λ> interleave 1 [2]
[[1,2],[2,1]]

   interleave 1 [2:[]] = (1:2:[]) : map (2:) (interleave 1 [])
                       = (1:2:[]) : map (2:) [[ 1 ]]
                       = (1:2:[]) : [[ 2, 1 ]]    -- Note that this will error if don't have parens around first group
                       = [[1,2],[2,1]]

Then try suppose we do interleave 1 [2, 3] == [[1,2,3], [2,1,3], [2,3,1]] ...
-}