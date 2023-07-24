module Main where

import qualified Data.Map as M
import Morse
import Test.QuickCheck

-- generators
allowedChars :: [Char]
allowedChars = M.keys letterToMorse

allowedMorse :: [Morse]
allowedMorse = M.elems letterToMorse

charGen :: Gen Char
charGen = elements allowedChars

morseGen :: Gen Morse
morseGen = elements allowedMorse

-- props
prop_roundTrip :: Property
prop_roundTrip =
    forAll charGen $
        \c ->
            (charToMorse c >>= morseToChar)
            == Just c
{- Reminder
charToMorse :: Char -> Maybe Morse
-}

main :: IO ()
main = quickCheck prop_roundTrip