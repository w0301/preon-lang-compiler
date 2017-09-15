module Parser.Expressions where

import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Char
import Text.Parsec.Token
import Text.Parsec.String

import Parser.Common
import Parser.Constants

data Expression
  = FunctionCall String [Expression]
  | OperatorCall String Expression Expression
  | Condition Expression Expression Expression
  | ConstantCall Constant
  | TypeCreatorCall String [Expression]
  deriving Show

parseExpression :: Parser Expression
parseExpression =
  try (
    try (spaces >> parseCondition)
    <|>
    try (spaces >> parseNoArgFunctionCall)
    <|>
    try (spaces >> parseConstantCall)
    <|>
    try (parenthesis parseFunctionCall)
    <|>
    try (parenthesis parseExpression)
  )

parseNoArgFunctionCall :: Parser Expression
parseNoArgFunctionCall = do
  name <- parseFunctionName
  return $ FunctionCall name []

parseConstantCall :: Parser Expression
parseConstantCall = do
  constant <- parseConstant
  return $ ConstantCall constant

parseFunctionCall :: Parser Expression
parseFunctionCall = do
  name <- parseFunctionName
  args <- many parseExpression
  return $ FunctionCall name args

parseCondition :: Parser Expression
parseCondition = do
  spaces >> string "if" >> spaces
  condExp <- parseExpression
  spaces >> string "then" >> spaces
  thenExp <- parseExpression
  spaces >> string "else" >> spaces
  elseExp <- parseExpression
  return $ Condition condExp thenExp elseExp

parseFunctionName :: Parser String
parseFunctionName = do
  firstLetter <- lower
  rest <- many (letter <|> digit)
  return $ [firstLetter] ++ rest

parseTypeCreatorName :: Parser String
parseTypeCreatorName = do
  firstLetter <- upper
  rest <- many (letter <|> digit)
  return $ [firstLetter] ++ rest
