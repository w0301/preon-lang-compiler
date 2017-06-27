module Main where

import Parser
import System.IO
import Text.Parsec

testParser :: String -> String
testParser input = case parse parseExpression "expression" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found value: " ++ show val

main :: IO ()
main = do
  putStrLn ("Parse: " ++ testParser "( test1 (test2 (test3 t) tt) )")
