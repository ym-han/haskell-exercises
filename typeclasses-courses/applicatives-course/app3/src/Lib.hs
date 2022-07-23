{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use if" #-}
{-# HLINT ignore "Redundant bracket" #-}
module Lib where

-- the following is from typeclasses.com

import Data.Char
import Data.Validation
import Data.Semigroup

newtype Password = Password String
    deriving (Eq, Show)

newtype Error = Error [String]
    deriving (Eq, Show)

newtype Username = Username String
    deriving (Eq, Show)

instance Semigroup Error
  where
    Error xs <> Error ys = Error (xs <> ys)

data User = User Username Password
    deriving (Eq, Show)

passwordLength :: String -> Validation Error Password
passwordLength "" = Failure (Error ["Your password cannot be empty."])
passwordLength password =
    case (length password > 20) of
        True -> Failure (Error ["Your password cannot be longer than 20 characters."])
        False -> Success (Password password)

usernameLength :: String -> Validation Error Username
usernameLength name =
    case (length name > 15) of
        True -> Failure (Error ["Username cannot be longer than 15 characters."])
        False -> Success (Username name)

allAlpha :: String -> Validation Error String
allAlpha "" = Failure (Error ["Cannot be left blank."])
allAlpha xs =
    case (all isAlphaNum xs) of
        False -> Failure (Error ["You may not use white space or special characters."])
        True -> Success xs

stripSpace :: String -> Validation Error String
stripSpace "" = Failure (Error ["Cannot be left blank."])
stripSpace (x:xs) =
    case (isSpace x) of
        True -> stripSpace xs
        False -> Success (x:xs)

validatePassword :: Password -> Validation Error Password
validatePassword (Password password) =
    case (stripSpace password) of
        Failure err -> Failure err
        Success password' -> allAlpha password' *> passwordLength password'

validateUsername :: Username -> Validation Error Username
validateUsername (Username username) =
    case (stripSpace username) of
        Failure err -> Failure err
        Success name -> allAlpha name *> usernameLength name

makeUser :: Username -> Password -> Validation Error User
makeUser name password =
    User <$> validateUsername name <*> validatePassword password

