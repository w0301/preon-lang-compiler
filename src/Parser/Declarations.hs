module Parser.Declarations where

import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Char
import Text.Parsec.Token
import Text.Parsec.String


data Declaration
  = AbstractObjectDefinition String [FieldDeclaration]
  | ObjectDefinition String [FieldDeclaration]
  deriving Show

data FieldDeclaration
  = NormalField String [Statement]
  | ConstantField String [Statement]
  | FunctionField String [Statement]
  deriving Show
