module Chp3Exs where

-- Try implementing concatMap using foldl and foldr. Which one is better? Why?


-- Filling in typeholes (p. 115)
{-
Using type holes, try to fill in the value of undefined 
so that you get the following output: 
λ example [5..15]
"spftqgurhvsuwtjxukyblzcmadnbeacfp"
-}
mapApply :: [a -> b] -> [a] -> [b]
mapApply toApply =
  concatMap (\input -> map ($ input) toApply)

example :: [Int] -> String 
example = mapApply (map (lookupLetter . ) offsets)
-- could also use [ lookupLetter . offFn | offFn <- offsets]
  where
    letters :: [Char] 
    letters = ['a'..'z']
    
    lookupLetter :: Int -> Char 
    lookupLetter n = letters !! n

    offsets :: [Int -> Int]
    offsets = [rot13, swap10, mixupVowels]

    rot13 :: Int -> Int
    rot13 n = (n + 13) `rem` 26

    swap10 :: Int -> Int 
    swap10 n
      | n <= 10 = n + 10
      | n <= 20 = n - 10
      | otherwise = n

    mixupVowels :: Int -> Int 
    mixupVowels n =
      case n of 
        0 -> 8
        4 -> 14
        8 -> 20
        14 -> 0
        20 -> 4
        n' -> n'

