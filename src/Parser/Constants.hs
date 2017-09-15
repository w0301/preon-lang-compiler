module Parser.Constants where

import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Char
import Text.Parsec.Token
import Text.Parsec.String

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
