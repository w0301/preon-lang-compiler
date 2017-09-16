module Parser.Program where

import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Char
import Text.Parsec.Token
import Text.Parsec.String

import Parser.Declarations


data Program
  = Program [ObjectDeclaration]
  deriving Show

parseProgram :: Parser Program
parseProgram = fmap Program (many parseObjectDeclaration)

mergePrograms :: Program -> Program -> Program
mergePrograms (Program decls1) (Program decls2) = Program (decls1 ++ decls2)
