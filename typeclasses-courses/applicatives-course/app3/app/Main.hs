module Main where

import Lib

main :: IO ()
main =
  do
    putStrLn "Please enter a username."
    username <- Username <$> getLine
    putStrLn "Please enter a password."
    password <- Password <$> getLine
    print (makeUser username password)
