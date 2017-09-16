module Main where

import System.IO
import Text.Parsec

import Parser.Program
import Parser.Expressions
import Parser.Declarations

testParser :: String -> String
testParser input = case parse parseExpression "expression" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found value: " ++ show val

main :: IO ()
main = do
  putStrLn ("Parse: " ++ testParser "( test1 (test2 (test3 if t then 5 else 10) tt) )")
