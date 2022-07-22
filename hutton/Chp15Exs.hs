module Chp15Exs where

-- generating primes
infinite = [2..]

findPrimes :: Integral a => [a] -> [a]
findPrimes (x:xs) = x : findPrimes (filter (\elt -> elt `mod` x /= 0) xs) 

primes = findPrimes infinite 

-- take 5 (primes)
-- takeWhile (<35) primes

-- The lesson from Hutton: 
-- separating the data part (the potntially infinite list) from the control part (take 5 ...)
-- can easily swap out the control part, depending on what we want to do with the data!
