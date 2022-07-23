{-# LANGUAGE OverloadedStrings #-}

import Lib

import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

--  A property test has the Property type
bird_direction :: Property
bird_direction = 
    property $ do
        

tests :: IO Bool
tests =
  checkParallel $ Group "Applicatives Lesson 3 Ex. 1: *> vs <* doesn't matter for allAlpha and length checking" [
      ("prop_reverse", prop_reverse)
    ]


-- referred to hedgehog tutorial at https://typeclasses.com/functortown/breaking-the-laws