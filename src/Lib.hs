{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RankNTypes #-}
module Lib
    ( Validation(), Underlying(value), Validatable(validation), Validated,
     withMessage, changeMessage, apply, check, validate
    ) where

newtype Validation e a b = Validation (a -> Either [e] b) 

instance Functor (Validation e a) where 
    fmap f (Validation val) = Validation $ fmap f . val
instance Applicative (Validation e a) where  
    pure = Validation . const . Right
    (Validation f) <*> (Validation x) = 
        Validation
        (\y -> case (f y, x y) of
            (Left e,Left e2) -> Left (e ++ e2)
            (Left e, Right _) -> Left e
            (Right _,Left e) -> Left e
            (Right g, Right z) -> Right (g z)
        )

check :: (a -> b) -> Validation e b c -> Validation e a c
check f (Validation val) = Validation (val . f)

withMessage :: (a -> Maybe b) -> e -> Validation e a b
withMessage predicate message = Validation val where
    val a = case predicate a of 
        Just b -> Right b
        Nothing -> Left [message]

changeMessage :: Validation e a b -> e -> Validation e a b
changeMessage (Validation val) msg = Validation $ \x -> case val x of
    Left y -> Left [msg]
    Right y -> Right y

apply :: Validation e a b -> a -> Either [e] b
apply (Validation val) = val

type Validated b e = forall r . (b -> r) -> ([e] -> r) -> r

validate :: Validatable e a b => a -> Validated b e
validate x valid error = case validation `apply` x of
    Right x -> valid x
    Left x -> error x

class Underlying a b | b -> a where
    value :: b -> a
class Validatable e a b | b -> a e where 
    validation :: Validation e a b
