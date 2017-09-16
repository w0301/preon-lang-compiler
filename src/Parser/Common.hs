module Parser.Common where

import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Char
import Text.Parsec.Token
import Text.Parsec.String

parenthesis :: Parser a -> Parser a
parenthesis subParser = do
  spaces >> char '(' >> spaces
  res <- subParser
  spaces >> char ')' >> spaces
  return res

simpleName :: Parser String
simpleName = do
  firstChar <- letter
  restChars <- (many alphaNum)
  return $ [firstChar] ++ restChars
