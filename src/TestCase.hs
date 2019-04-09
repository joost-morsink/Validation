{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module TestCase (Email(), NonEmptyString(), parseEmail, parseNonEmptyString, nonEmptyStringValidation, PersonDto(PersonDto), Person) where

import Lib
import System.IO
import Control.Monad

newtype Email = Email String deriving (Show)
newtype NonEmptyString = NonEmptyString String deriving (Show)

instance Underlying String Email where
    value (Email e) = e
instance Validatable String String Email where
    validation = parseEmail `withMessage` "Cannot parse email."

parseEmail :: String -> Maybe Email
parseEmail str = if '@' `elem` str && '.' `elem` str then Just $ Email str else Nothing

parseNonEmptyString :: String -> Maybe NonEmptyString
parseNonEmptyString [] = Nothing
parseNonEmptyString str = Just . NonEmptyString $ str

instance Underlying String NonEmptyString where 
    value (NonEmptyString str) = str
instance Validatable String String NonEmptyString where
    validation = parseNonEmptyString `withMessage` "String is required."

nonEmptyStringValidation :: String -> Validation String String NonEmptyString
nonEmptyStringValidation msg = parseNonEmptyString `withMessage` msg

data PersonDto = PersonDto { dtoFirstName :: String, dtoLastName :: String, dtoEmail :: String }
    deriving (Show)

data Person = Person { firstName :: NonEmptyString, lastName :: NonEmptyString, email :: Email }
instance Show Person where
    show p = value (firstName p) ++ " " ++ value (lastName p) ++ " <" ++ value (email p) ++ ">"

instance Validatable String PersonDto Person where
    validation =
        Person 
            <$> check dtoFirstName (validation `changeMessage` "FirstName is empty") 
            <*> check dtoLastName (validation `changeMessage` "Lastname is empty") 
            <*> check dtoEmail validation

