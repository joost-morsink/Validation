{-# LANGUAGE RankNTypes #-}

module Main where

import qualified Lib as L(Validated)
import Lib hiding (Validated)
import TestCase
import System.IO
import Control.Monad

type Validated a = L.Validated a String

ask :: String -> IO String
ask question = do
    putStr $ question ++ " "
    hFlush stdout
    getLine

main :: IO ()
main = do
    fn <- ask "First name?"
    ln <- ask "Last name?"
    em <- ask "Email?"
    let pdto = PersonDto fn ln em 
    print pdto
    let p = validate pdto :: Validated Person
    p print (mapM_ putStrLn) 
    putStrLn "Doei!"
