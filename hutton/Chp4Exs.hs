module Chp4Exs where

-- split even lengthed list into two halves
halve :: [a] -> ([a], [a])
halve lst = splitAt (length lst `div` 2) lst  



-- returns third elt in a list, where list has at least 3 elts
-- a. using head and tail

thirdWithHT :: [a] -> a
thirdWithHT lst = (head . tail . tail) lst

-- b. using list idxing
thirdWithIdxing :: [a] -> a
thirdWithIdxing lst = lst !! 2

-- c. pattern matching
-- had to look at answers for this one. makes sense in retrospect


-- safetail
-- a. conditional expression
safetailCond :: [a] -> [a]
safetailCond xs = if null xs then xs else tail xs

-- b. guarded equations
safetailGuard :: [a] -> [a]
safetailGuard xs 
  | null xs = xs
  | otherwise = tail xs

-- c. pattaern matching
safetailPM :: [a] -> [a]
safetailPM [] = []
safetailPM (_:xs) = xs






