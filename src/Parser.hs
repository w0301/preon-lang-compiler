module Parser where

import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Char
import Text.Parsec.Token
import Text.Parsec.String

-- Constants parsing --

data Constant
  = IntegerConstant Integer
  | FloatConstant Double
  | CharConstant Char
  | StringConstant String
  deriving Show

parseConstant :: Parser Constant
parseConstant =
  fmap FloatConstant (try parseFloatConstant)
  <|>
  fmap IntegerConstant (try parseIntegerConstant)
  <|>
  fmap CharConstant (try parseCharConstant)
  <|>
  fmap StringConstant (try parseStringConstant)

parseIntegerConstant :: Parser Integer
parseIntegerConstant = do
  char '-'
  num <- parseInteger
  return $ -num
  <|> parseInteger
  where
    parseInteger = do
      numStr <- (many1 digit)
      return $ read numStr

parseFloatConstant :: Parser Double
parseFloatConstant = do
  char '-'
  num <- parseFloat
  return $ -num
  <|> parseFloat
  where
    parseFloat = do
      numStr1 <- (many1 digit)
      dot <- char '.'
      numStr2 <- (many1 digit)
      return $ read (numStr1 ++ [dot] ++ numStr2)

-- TODO : allow escaping of '
parseCharConstant :: Parser Char
parseCharConstant = do
  char '\''
  ch <- noneOf "\'"
  char '\''
  return $ ch

-- TODO : allow escaping of "
parseStringConstant :: Parser String
parseStringConstant = do
  char '"'
  str <- many (noneOf "\"")
  char '"'
  return $ str


-- Expressions parsing --

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
    try (parenthesis parseFunctionCall)
    <|>
    try (parenthesis parseExpression)
  )

parenthesis :: Parser a -> Parser a
parenthesis subParser = do
  spaces >> char '(' >> spaces
  res <- subParser
  spaces >> char ')' >> spaces
  return res

parseNoArgFunctionCall :: Parser Expression
parseNoArgFunctionCall = do
  name <- parseFunctionName
  return $ FunctionCall name []

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
