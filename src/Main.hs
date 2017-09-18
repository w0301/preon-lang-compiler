module Main where

import System.IO
import Text.Parsec

import Parser.Program
import Parser.Expressions
import Parser.Declarations

testParser :: String -> String
testParser input = case parse parseProgram "program" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found value: " ++ show val

main :: IO ()
main = do
  putStrLn ("Parse: " ++ testParser "object Foo end object Bar<T1, T2> \nf : Int -> Foo = |a, b, c| -> (asd)  \n end")
